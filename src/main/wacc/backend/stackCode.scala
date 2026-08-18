package backend
import wacc.renamer.Renamed
import wacc.ast.SemType


/* 
stackCode:
    - creates initial ir following ast
    - not specific to x86
    - IR 1
*/

sealed trait Instr

// PRINTER
object Printer {
    def print(instrs: List[Instr]): Unit = instrs.foreach(println)
}

// CONTROL
case class Label(id: Int) extends Instr
case class Jump(id: Int) extends Instr
case class JumpIfFalse(id: Int) extends Instr

// LITERALS
case class PushInt(value: Int) extends Instr
case class PushBool(value: Boolean) extends Instr
case class PushChar(value: Char) extends Instr
case class PushString(value: String) extends Instr
case object PushNull extends Instr

// VARIABLES
case class LoadVar(name: Renamed) extends Instr
case class StoreVar(name: Renamed) extends Instr

// ARRAYS
case class NewArrayI(elemTy: SemType) extends Instr
case class ArrayLoadI(elemTy: SemType) extends Instr
case class ArrayStoreI(elemTy: SemType) extends Instr
case object DupI extends Instr
case object DropI extends Instr

// PAIRS
case object NewPairI extends Instr
case object LoadFstI extends Instr
case object LoadSndI extends Instr
case object StoreFstI extends Instr
case object StoreSndI extends Instr

// EXPRESSIONS
case object AddI extends Instr
case object SubI extends Instr
case object MulI extends Instr
case object DivI extends Instr
case object ModI extends Instr

case object EqI extends Instr
case object NeI extends Instr
case object LtI extends Instr
case object LeI extends Instr
case object GtI extends Instr
case object GeI extends Instr

case object NotI extends Instr
case object NegI extends Instr
case object LenI extends Instr
case object OrdI extends Instr
case object ChrI extends Instr

// STATEMENTS
case object ExitI extends Instr
case class ReadI(ty: SemType) extends Instr
case object FreeI extends Instr
case object ReturnI extends Instr

// FUNCTIONS
case class FunctionLabel(name: String) extends Instr
case class CallI(name: String, argc: Int) extends Instr
case class EnterFrameI(params: Int, locals: Int) extends Instr
case object LeaveFrameI extends Instr

// IO
case class PrintI(ty: SemType, nl: Boolean) extends Instr