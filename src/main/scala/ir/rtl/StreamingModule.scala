/*
 *    _____ ______          SGen - A Generator of Streaming Hardware
 *   / ___// ____/__  ____  Department of Computer Science, ETH Zurich, Switzerland
 *   \__ \/ / __/ _ \/ __ \
 *  ___/ / /_/ /  __/ / / / Copyright (C) 2020-2025 François Serre (serref@inf.ethz.ch)
 * /____/\____/\___/_/ /_/  https://github.com/fserre/sgen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 */

package ir.rtl

import ir.rtl.hardwaretype.HW
import ir.rtl._
import ir.spl.SPL

import java.io.PrintWriter
import scala.collection.mutable
import scala.sys.process._


/** Streaming-module DSL.
 * This DSL consists of nodes called streaming modules, i.e. hardware modules that correspond to an SPL operator. They have as many data inputs as outputs, each of the same datatype, and use a token based synchronisation system.
 * This DSL allows to combine streaming modules, by composing them, or using tensor product.
 * */
abstract class StreamingModule[U: HW](val t: Int, val k: Int) extends Module:
  final val n: Int = t + k
  final val N: Int = 1 << n
  final val K: Int = 1 << k
  final val T: Int = 1 << t
  final val hw = HW[U]

  def spl: SPL[U]

  override lazy val name: String = spl.getClass.getSimpleName.toLowerCase

  override def description: Iterator[String] = io.Source.fromResource("streaming.txt").getLines().
    filterNot(s=> s.contains ("full-throughput") && minGap!=0).
    filterNot(s=> s.contains ("requires a delay") && minGap==0).
    filterNot(s=>(s.contains ("single RAM control") || s.contains("additional cycles") || s.contains ("-dualRAMcontrol")) && !hasSinglePortedMem).
    filterNot(s=>(s.contains("index_out") || s.contains("valid_out")) && !StreamingModule.indexOutputs).
    map(_.
    replace("SIZE",N.toString).
    replace("DATADURATION",T.toString).
    replace("STREAMINGWIDTH",K.toString).
      replace("LATENCY",(latency + inputDelay).toString).
      replace("TOTALGAP",(minGap+T).toString).
      replace("GAP",minGap.toString).
      replace("INPUTS",s"i0 - i${K-1}").
      replace("OUTPUTS",s"o0 - o${K-1}").
      replace("START",if(nextAt==0) "at the same time as" else if(nextAt>0) s"$nextAt cycles after" else s"${-nextAt} cycles before").
      replace("HW",hw.description)
  )

  def implement(rst: Component, token: Int => Component, inputs: Seq[Component]): Seq[Component]

  def latency: Int

  def minGap = 0

  def hasSinglePortedMem:Boolean=false

  final lazy val dataInputs: Vector[Input] = Vector.tabulate(K)(i => Input(hw.size, "i" + i))
  final val reset = Input(1, "reset")
  final val next = Input(1, "next")

  def *(rhs: StreamingModule[U]): StreamingModule[U] = Product(this, rhs)

  final override lazy val inputs: Seq[Input] = reset +: next +: dataInputs
  final override lazy val outputs: Seq[Output] =
    val tokens = mutable.Map[Int, Wire]()

    def getToken(time: Int) = tokens.getOrElseUpdate(time, Wire(1))

    // The data inputs and the reset enter through a register of their own, kept in the fabric (see inputDelay): a
    // streaming module's first operation otherwise works straight off the port, and the registers it then depends on
    // belong to the instantiating design, placed wherever that design put them (the inverse N=4096 core's first-stage
    // adders, 17 failing endpoints at 625 MHz in build e0c578e).
    val rst = reset.keepRegister
    // The data inputs go through wires, so that they can be delayed once the time at which the token is needed is known.
    val dataWires = dataInputs.map(i => Wire(i.size))
    val res = implement(rst, getToken, dataWires).zipWithIndex.map { case (comp, i) => Output(comp, "o" + i) }
    val next_out = Output(getToken(latency), "next_out")

    // Time (in cycles after the first input) at which the token is first needed by the control logic.
    val minTime = tokens.keys.min
    // With StreamingModule.alignNext, next is asserted with the first input: if the control logic needs the token before the first
    // input, the data is delayed accordingly (at the cost of registers or shift-register LUTs), and if it needs it after, the token is.
    // The input register is the first of that delay, or the whole of it when the control logic does not need one.
    val alignDelay = if StreamingModule.alignNext then math.max(0, -minTime) else 0
    val delay = math.max(alignDelay, 1)
    dataWires.zip(dataInputs).foreach((w, i) => w.input = i.keepRegister.delay(delay - 1))
    val tokenStart = (if StreamingModule.alignNext then math.min(minTime, 0) else minTime) - (delay - alignDelay)

    tokens.toSeq.sortBy(_._1).foldLeft[(Int,Component)]((tokenStart,next)){case ((prevTime, prevComp),(time, wire)) =>
      val diff= time-prevTime
      assert(diff>=0)
      val res = if diff>0 then Register(prevComp, diff) else prevComp
      wire.input = res
      (time, res)}

    _nextAt = Some(if StreamingModule.alignNext then 0 else minTime)
    _inputDelay = Some(delay)

    // Index of the current output within its dataset, and whether the outputs are part of a dataset (StreamingModule.indexOutputs).
    val indexOutputs = if StreamingModule.indexOutputs && t > 0 then
      val token = next_out.input
      val count = new Wire(t) // value of the index at the previous cycle, plus one
      val index = Mux(token, Seq(count, Const(t, 0)))
      count.input = Register(Plus(Seq(index, Const(t, 1))))
      val active = new Wire(1)
      val valid = Or(Seq(token, active))
      val activeNext = Mux(token, Seq(Mux(Equals(index, Const(t, T - 1)), Seq(active, Const(1, 0))), Const(1, 1)))
      active.input = Register(Mux(rst, Seq(activeNext, Const(1, 0)))) // cleared by reset, so that valid_out is defined before the first dataset
      Seq(Output(index, "index_out"), Output(valid, "valid_out"))
    else Seq()

    next_out +: res :++ indexOutputs


  final lazy val dataOutputs: Seq[Output] = outputs.drop(1).take(K)

  private var _inputDelay: Option[Int] = None

  /** Number of cycles the inputs are delayed by before entering the design: at least one (the input register), more
   *  when the control logic needs the token before the first input (see StreamingModule.alignNext) */
  final def inputDelay: Int =
    if (_inputDelay.isEmpty) outputs
    _inputDelay.get

  final lazy val next_out: Output = outputs.head

  private var _nextAt: Option[Int] = None

  final def nextAt: Int =
    if (_nextAt.isEmpty) outputs
    _nextAt.get

  //final def eval(inputs: Seq[BigInt], set: Int): Seq[BigInt] = spl.eval(inputs.map(hw.valueOf), set).map(hw.bitsOf)

  def testBenchInput(repeat:Int): Seq[U]=(0 until repeat*N).map(i=>hw.num.fromInt(i))

object StreamingModule:
  /** Whether next is asserted together with the first input of a dataset, rather than when the control logic first needs it */
  var alignNext: Boolean = false

  /** Whether the design has index_out and valid_out outputs (index of the current output within its dataset, and whether it is part of one) */
  var indexOutputs: Boolean = false
