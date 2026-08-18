package backend

/* 
x86IR:
    - creates enums and classes used in x86Lowerer
    - helps avoid magic numbers
    - IR 2
*/

// Regs:
sealed trait x86Reg

// 64 bits
case object RDI extends x86Reg
case object RSP extends x86Reg
case object RBP extends x86Reg
case object RDX extends x86Reg
case object RBX extends x86Reg
case object RAX extends x86Reg
case object RSI extends x86Reg
case object RCX extends x86Reg

// 32 bits
case object EAX extends x86Reg
case object ESI extends x86Reg 
case object EDI extends x86Reg

case object AL extends x86Reg

// Memory size dependent on types
sealed trait MemSize
case object ByteM  extends MemSize
case object DwordM extends MemSize
case object QwordM extends MemSize

// Types of operand
sealed trait Op 
object Op {
    final case class Imm(value: Int) extends Op
    final case class R(reg: x86Reg) extends Op
    final case class SizedMem(base: R, offset: Int, size: MemSize) extends Op
}

// All instructions in x86 required
sealed trait x86Instr

// Included at start of assembly:
case object IntelSyntax extends x86Instr
case object GloblMain extends x86Instr
case object SectionRodata extends x86Instr
case object SectionText extends x86Instr
final case class RodataInt(value: Int) extends x86Instr
final case class RodataString(label: x86Label, value: String) extends x86Instr

// Labels
sealed trait x86Label
case class NamedLabel(name: String) extends x86Label
case class LocalLabel(id: Int) extends x86Label

// General Instructions
case class LabelDef(label: x86Label) extends x86Instr
final case class Push(op: Op) extends x86Instr
final case class Pop(op: Op) extends x86Instr
final case class Add(dst: Op, src: Op) extends x86Instr
final case class Sub(dst: Op, src: Op) extends x86Instr
final case class IMul(dst: Op, src: Op) extends x86Instr
final case class IDiv(src: Op.R) extends x86Instr
final case class Neg(op: Op.R) extends x86Instr
final case class Xor(dst: Op, src: Op) extends x86Instr
final case class SetCC(cond: Condition, dst: Op) extends x86Instr
final case class Movzx(dst: Op, src: Op) extends x86Instr
final case class Call(label: NamedLabel) extends x86Instr
final case class And(dst: Op, src: Op) extends x86Instr
final case class Mov(dst: Op, src: Op) extends x86Instr
final case class Lea(dst: Op, src: x86Label) extends x86Instr
final case class MovAl(v: Int) extends x86Instr
case object Ret extends x86Instr
case object Cqo extends x86Instr
case object Cdqe extends x86Instr
case class Cmp(a: Op, b: Op) extends x86Instr
case class Jmp(cond: Option[Condition], label: x86Label) extends x86Instr

// Removing Some and None when using Jmp, two constuctors
object Jmp:
    def apply(label: x86Label): Jmp = Jmp(None, label)
    def apply(cond: Condition, label: x86Label): Jmp = Jmp(Some(cond), label)

// Condition for comparisons (jmp, setCC)
sealed trait Condition
case object EqualC    extends Condition
case object NotEqualC extends Condition
case object LessC     extends Condition
case object GreaterC  extends Condition
case object LessEqC   extends Condition
case object GreaterEqC extends Condition
case object OverflowC extends Condition

// All possible support routines
sealed trait SupportRoutine
case object ExitRoutine        extends SupportRoutine
case object PrintIntRoutine    extends SupportRoutine
case object PrintBoolRoutine   extends SupportRoutine
case object PrintCharRoutine   extends SupportRoutine
case object PrintStringRoutine extends SupportRoutine
case object PrintRefRoutine    extends SupportRoutine
case object PrintLnRoutine     extends SupportRoutine
case object ReadIntRoutine     extends SupportRoutine
case object ReadCharRoutine    extends SupportRoutine
case object OverflowRoutine    extends SupportRoutine
case object DivZeroRoutine     extends SupportRoutine
case object NullDerefRoutine   extends SupportRoutine
case object BoundsRoutine      extends SupportRoutine

// All labels - avoid magic
object Labels:
    val exit      = NamedLabel("_exit")
    val exitPlt   = NamedLabel("exit@plt")
    val prints    = NamedLabel("_prints")
    val printsFmt = NamedLabel(".L._prints_str0")
    val printi    = NamedLabel("_printi")
    val printiFmt = NamedLabel(".L._printi_str0")
    val printb    = NamedLabel("_printb")
    val printbFmt   = NamedLabel(".L._printb_str2")
    val printbTrue  = NamedLabel(".L._printb_str1")
    val printbFalse = NamedLabel(".L._printb_str0")
    val printbEnd   = NamedLabel(".L._printb_end")
    val printc    = NamedLabel("_printc")
    val printcFmt = NamedLabel(".L._printc_str0")
    val printp    = NamedLabel("_printp")
    val printpFmt = NamedLabel(".L._printp_str0")
    val println   = NamedLabel("_println")
    val printlnFmt = NamedLabel(".L._println_str0")
    val main      = NamedLabel("main")
    val printf    = NamedLabel("printf@plt")
    val puts      = NamedLabel("puts@plt")
    val fflush    = NamedLabel("fflush@plt")
    val malloc    = NamedLabel("malloc@plt")
    val free      = NamedLabel("free@plt")
    val scanf     = NamedLabel("scanf@plt")
    val readiFmt  = NamedLabel(".L._readi_str0")
    val readcFmt  = NamedLabel(".L._readc_str0")
    val readi     = NamedLabel("_readi")
    val readc     = NamedLabel("_readc")
    val overflow      = NamedLabel("_errOverflow")
    val overflowMsg   = NamedLabel(".L._errOverflow_str0")
    val divZero       = NamedLabel("_errDivZero")
    val divZeroMsg    = NamedLabel(".L._errDivZero_str0")
    val nullDeref     = NamedLabel("_errNull")
    val nullDerefMsg  = NamedLabel(".L._errNull_str0")
    val boundsUp      = NamedLabel("_errBoundsUp")
    val boundsUpMsg   = NamedLabel(".L._errBoundsUp_str0")
    val boundsNeg     = NamedLabel("_errBoundsNeg")
    val boundsNegMsg  = NamedLabel(".L._errBoundsNeg_str0")

// Print format types - avoid magic
object PrintFormat:
    val int      = "%d"
    val char     = "%c"
    val ref      = "%p"
    val string   = "%.*s"
    val boolTrue  = "true"
    val boolFalse = "false"
    val newline  = ""

// Regs with useful name - avoid magic
object Regs:
    // argument registers
    val arg1    = Op.R(RDI)
    val arg2    = Op.R(RSI)
    val arg3    = Op.R(RDX)
    val arg4    = Op.R(RCX)
    val arg1_32 = Op.R(EDI)
    val arg2_32 = Op.R(ESI)
    val ret32  = Op.R(EAX)
    val retLow = Op.R(AL)

    // frame
    val stackP = Op.R(RSP)
    val baseP = Op.R(RBP)

    // callee-saved
    val tmp0 = Op.R(RAX)
    val tmp1 = Op.R(RBX)
    val tmp2 = Op.R(RCX)