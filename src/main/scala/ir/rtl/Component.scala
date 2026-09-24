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

/**
 * Class that represent a node on an RTL graph
 *
 * @param size Size of the node in bits
 * @param _parents Parent nodes
 */
abstract sealed class Component(val size: Int, _parents: Component*):
  /**
   * Parent nodes
   */
  def parents: Seq[Component] = _parents.toSeq

  /**
   * Returns a register node of the current node
   */
  final def register = Register(this)

  final def delay(cycles:Int) =
    require(cycles>=0)
    if cycles == 0 then
      this
    else
      Register(this, cycles)

abstract sealed class ImmutableComponent(size: Int, _parents: Component*) extends Component(size, _parents*):
  override val hashCode = (parents +: this.getClass.getSimpleName).hashCode()

final class Wire(override val size: Int) extends Component(size):
  private var _input: Option[Component] = None

  def input_=(comp: Component): Unit = {
    assert(_input.isEmpty)
    assert(comp.size == size)
    _input = Some(comp)
  }

  def input: Component = _input.get

  override def parents = Seq(input)

  override val hashCode = Seq("Wire", size).hashCode()

  /**
   * Checks for equality. We only check for reference equality of the input to prevent endless loop in case of cycles within the graph.  
   */
  override def equals(that: Any) = that match
    case that:AnyRef => (this eq that) || (that match
      case that: Wire => that.input eq input
      case _ => false)
    case _ => false


object Wire :
  def apply(size: Int): Wire = new Wire(size)

  def unapply(arg: Wire): Option[Component] = Some(arg.input)



case class Const(override val size: Int, value: BigInt) extends ImmutableComponent(size):
  override val hashCode = value.hashCode()

case class Register(input: Component, cycles: Int = 1) extends Component(input.size, input):
  require(cycles>0, s"Wrong delay:$cycles")

case class Input(override val size: Int, name: String) extends ImmutableComponent(size):
  override val hashCode = name.hashCode()

case class Output(input: Component, name: String) extends ImmutableComponent(input.size, input)

case class Plus(terms: Seq[Component]) extends ImmutableComponent(terms.head.size, terms*)

case class Minus(lhs: Component, rhs: Component) extends ImmutableComponent(lhs.size, lhs, rhs)

case class Times(lhs: Component, rhs: Component) extends ImmutableComponent(lhs.size + rhs.size, lhs, rhs)

case class And(terms: Seq[Component]) extends ImmutableComponent(terms.head.size, terms*)

case class Xor(inputs: Seq[Component]) extends ImmutableComponent(inputs.head.size, inputs*)

case class Or(inputs: Seq[Component]) extends ImmutableComponent(inputs.head.size, inputs*)

case class Not(input: Component) extends ImmutableComponent(input.size, input)

case class Equals(lhs: Component, rhs: Component) extends ImmutableComponent(1, lhs, rhs)

case class Mux(address: Component, inputs: Seq[Component]) extends ImmutableComponent(inputs.head.size, address +: inputs*)

case class Concat(inputs: Seq[Component]) extends ImmutableComponent(inputs.map(_.size).sum, inputs*)

case class Tap(input: Component, range: Range) extends ImmutableComponent(range.size, input)

/**
 * Simple dual-port RAM (one write port, one read port) with a fixed read latency of RAM.readLatency cycles.
 * The implementation (block or distributed memory, and the registers needed to reach this latency) is chosen by the backend
 * from the depth of the memory, so that it does not affect the timing model of the design.
 */
case class RAM(data: Component, wr: Component, rd: Component) extends ImmutableComponent(data.size, data, wr, rd):
  /** Whether this RAM is implemented in block memory (using its output register) rather than in distributed memory */
  def isBlock: Boolean = (1 << rd.size) >= RAM.blockDepth

object RAM:
  /** Number of cycles between the read address and the corresponding data at the output of the RAM */
  val readLatency = 2

  /** Minimum depth for a RAM or ROM to be implemented in block memory (shallower ones use distributed memory) */
  var blockDepth: Int = 128

/**
 * Read-only memory implemented in a block RAM primitive, with a read latency of RAM.readLatency cycles.
 *
 * @param values Content of the memory
 * @param address Read address
 * @param size Width of the words
 */
case class BlockROM(values: Seq[BigInt], address: Component, override val size: Int) extends ImmutableComponent(size, address)

/**
 * Configuration of a block RAM primitive used as a simple dual-port RAM (one write port, one read port) or as a ROM.
 *
 * @param prim Name of the primitive (RAMB18E2 or RAMB36E2)
 * @param modeDepth Depth of the primitive in this configuration
 * @param width Width of a word (including parity bits)
 * @param sdp Whether the primitive is in simple dual-port mode (widest words, both halves of the primitive form one port)
 */
case class BlockRAMConfig(prim: String, modeDepth: Int, width: Int, sdp: Boolean):
  val dataBits: Int = width * 8 / 9
  val parityBits: Int = width - dataBits
  val addrBits: Int = if prim == "RAMB18E2" then 14 else 15
  val wordAddrBits: Int = BigInt(modeDepth - 1).bitLength
  val addrShift: Int = addrBits - wordAddrBits
  val tiles: Double = if prim == "RAMB18E2" then 0.5 else 1

object BlockRAMConfig:
  val all: Seq[BlockRAMConfig] = Seq(
    BlockRAMConfig("RAMB18E2", 512, 36, true), BlockRAMConfig("RAMB36E2", 512, 72, true),
    BlockRAMConfig("RAMB18E2", 1024, 18, false), BlockRAMConfig("RAMB36E2", 1024, 36, false),
    BlockRAMConfig("RAMB18E2", 2048, 9, false), BlockRAMConfig("RAMB36E2", 2048, 18, false),
    BlockRAMConfig("RAMB36E2", 4096, 9, false))

  /** Configuration for a memory of the given depth and width, if it should be implemented in block memory: fewest tiles first, then least unused depth. */
  def apply(depth: Int, width: Int): Option[BlockRAMConfig] =
    if depth < RAM.blockDepth then None
    else all.filter(_.modeDepth >= depth).minByOption(c => (((width + c.width - 1) / c.width) * c.tiles, c.modeDepth))

case class Extern(override val size:Int, filename:String, module:String, outputName:String, inputs:(String,Component)*) extends ImmutableComponent(size,inputs.map(_._2)*)

object ROM:
  def unapply(arg:Mux) =
    if arg.inputs.forall(_.isInstanceOf[Const]) then
      Some(arg.address,arg.inputs.map(_.asInstanceOf[Const].value))
    else
      None
