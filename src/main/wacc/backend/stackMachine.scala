package backend
import wacc.ast.Type
import wacc.TypedExpr
import wacc.TypedExpr.*
import wacc.TypedStmt
import wacc.TypedStmt.*
import wacc.TypedLValue
import wacc.TypedRValue
import wacc.TypedProgram
import wacc.TypedFunc
import scala.collection.mutable.ListBuffer

/* 
stackMachine:
    - converts ast to initial general ir
    - set up list of instructions to be converted to x86 instructions
    - Stage 2
*/
object stackMachine {
    private val arrTemp = wacc.renamer.Renamed("$array", renamedArrVal, Type.ArrayType(Type.CharType))
    private val idxTemp = wacc.renamer.Renamed("$index", renamedIdxVal, Type.IntType)
    private val lenTemp = wacc.renamer.Renamed("$length", renamedLenVal, Type.IntType)

    // Unwraps one layer of array type, returning inner type
    private def unwrapOnce(ty: wacc.ast.SemType): wacc.ast.SemType = ty match {
        case wacc.ast.Type.ArrayType(inner) => inner
        case _ => ty
    }

    // Compiles whole program to IR, emitting functions then main body
    def compileProgram(program: TypedProgram): List[Instr] = {
        given labeller: Labeller = Labeller()
        val out = ListBuffer[Instr]()
        program.funcs.foreach(func => compileFunc(func, out))
        out += FunctionLabel("main")
        out += EnterFrameI(zeroOffset, countLocals(program.body))
        compileStmt(program.body, out)
        out += PushInt(nullVal)
        out += ExitI
        out += LeaveFrameI
        out.toList
    }

    // Compiles a single function definition, storing params in reverse order
    private def compileFunc(func: TypedFunc, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        out += FunctionLabel(func.name)
        out += EnterFrameI(func.params.length, countLocals(func.body))
        func.params.reverse.foreach(param => out += StoreVar(param))
        compileStmt(func.body, out)
    }

    // Counts local variable slots needed for a statement
    private def countLocals(stmt: TypedStmt): Int = stmt match {
        case Declare(_, _, _) => declareCount
        case Seq(stmts) => stmts.map(countLocals).sum
        case Begin(stmt) => countLocals(stmt)
        case If(_, thenS, elseS) => countLocals(thenS) max countLocals(elseS)
        case While(_, body) => countLocals(body)
        case Print(expr, _) if expr.ty == Type.ArrayType(Type.CharType) => printCount
        case _ => defaultCount
    }

    // Compiles a statement to IR by dispatching on statement type
    private def compileStmt(stmt: TypedStmt, out: ListBuffer[Instr])(using labeller: Labeller): Unit = stmt match {
        case Skip => ()
        case Seq(stmts) => stmts.foreach(compileStmt(_, out))
        case Begin(stmt) => compileStmt(stmt, out)
        case Print(expr, nl) => compilePrint(expr, nl, out)
        case Exit(expr) => compileExpr(expr, out); out += ExitI
        case Declare(_, name, rVal) => compileRValue(rVal, out); out += StoreVar(name)
        case If(cond, thenS, elseS) => compileIf(cond, thenS, elseS, out)
        case While(cond, body) => compileWhile(cond, body, out)
        case Assign(lVal, rVal) => compileAssign(lVal, rVal, out)
        case Read(lVal) => compileRead(lVal, out)
        case Free(expr) => compileExpr(expr, out); out += FreeI
        case Return(expr) => compileExpr(expr, out); out += ReturnI
    }

    // Compiles a read statement, dispatching on lvalue kind
    private def compileRead(lVal: TypedLValue, out: ListBuffer[Instr])(using labeller: Labeller): Unit = lVal match {
        case TypedLValue.Ident(name) =>
            out += LoadVar(name)
            out += ReadI(lVal.ty)
            out += StoreVar(name)

        case TypedLValue.ArrayElem(arr, idx, _) =>
            out += LoadVar(arr)
            var currentTy: wacc.ast.SemType = arr.ty
            idx.init.foreach { index =>
                currentTy = unwrapOnce(currentTy)
                compileExpr(index, out)
                out += ArrayLoadI(currentTy)
            }
            currentTy = unwrapOnce(currentTy)
            compileExpr(idx.last, out)
            out += PushInt(nullVal)
            out += ReadI(lVal.ty)
            out += ArrayStoreI(currentTy)

        case TypedLValue.Fst(inner, _) => compileReadPair(inner, lVal, StoreFstI, out)

        case TypedLValue.Snd(inner, _) => compileReadPair(inner, lVal, StoreSndI, out)
    }

    // Compiles a read into a pair element using a given store instruction
    private def compileReadPair(inner: TypedLValue, lVal: TypedLValue, instr: Instr, out: ListBuffer[Instr])(using labeller: Labeller) = {
        compileLValueRef(inner, out)
        out += DupI
        // Dup the pointer before LoadFst so we still have it for the store after read
        out += LoadFstI
        out += DropI
        out += PushInt(nullVal)
        out += ReadI(lVal.ty)
        out += instr
    }

    // Compiles an assignment, dispatching on lvalue kind
    private def compileAssign(lVal: TypedLValue, rVal: TypedRValue, out: ListBuffer[Instr])(using labeller: Labeller): Unit = lVal match {
        case TypedLValue.Ident(name) =>
            compileRValue(rVal, out)
            out += StoreVar(name)

        case TypedLValue.ArrayElem(name, idx, _) =>
            out += LoadVar(name)
            var currentTy: wacc.ast.SemType = name.ty
            idx.init.foreach { index =>
                currentTy = unwrapOnce(currentTy)
                compileExpr(index, out)
                out += ArrayLoadI(currentTy)
            }
            currentTy = unwrapOnce(currentTy)
            compileExpr(idx.last, out)
            compileRValue(rVal, out)
            out += ArrayStoreI(currentTy)

        case TypedLValue.Fst(inner, _) => compileAssignPair(inner, rVal, StoreFstI, out)

        case TypedLValue.Snd(inner, _) => compileAssignPair(inner, rVal, StoreSndI, out)
    }

    // Compiles an assignment into a pair element using a given store instruction
    private def compileAssignPair(inner: TypedLValue, rVal: TypedRValue, instr: Instr, out: ListBuffer[Instr])(using labeller: Labeller) = {
        compileLValueRef(inner, out)
        compileRValue(rVal, out)
        out += instr
    }

    // Compiles a while loop with fresh labels for condition and exit
    private def compileWhile(cond: TypedExpr, body: TypedStmt, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        val whileLabel = labeller.generate()
        val endLabel = labeller.generate()
        out += Label(whileLabel)
        compileExpr(cond, out)
        out += JumpIfFalse(endLabel)
        compileStmt(body, out)
        out += Jump(whileLabel)
        out += Label(endLabel)
    }

    // Compiles an if-else with fresh labels for else and end branches
    private def compileIf(cond: TypedExpr, thenS: TypedStmt, elseS: TypedStmt, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        val elseLabel = labeller.generate()
        val endLabel = labeller.generate()
        compileExpr(cond, out)
        out += JumpIfFalse(elseLabel)
        compileStmt(thenS, out)
        out += Jump(endLabel)
        out += Label(elseLabel)
        compileStmt(elseS, out)
        out += Label(endLabel)
    }

    // Compiles a print statement, handling char arrays specially
    private def compilePrint(expr: TypedExpr, nl: Boolean, out: ListBuffer[Instr])(using labeller: Labeller): Unit = expr.ty match {
        case Type.ArrayType(Type.CharType) => compileCharArrayPrint(expr, nl, out)
        case _ =>
            compileExpr(expr, out)
            out += PrintI(expr.ty, nl)
    }

    // Compiles char array printing as an index-driven while loop
    // char arrays have no string representation so each element is printed individually
    private def compileCharArrayPrint(expr: TypedExpr, nl: Boolean, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        val whileLabel = labeller.generate()
        val endLabel = labeller.generate()
        compileExpr(expr, out)
        out += DupI
        out += LenI
        out += StoreVar(lenTemp)
        out += StoreVar(arrTemp)
        out += PushInt(nullVal)
        out += StoreVar(idxTemp)
        out += Label(whileLabel)
        out += LoadVar(idxTemp)
        out += LoadVar(lenTemp)
        out += LtI
        out += JumpIfFalse(endLabel)
        out += LoadVar(arrTemp)
        out += LoadVar(idxTemp)
        out += ArrayLoadI(Type.CharType)
        out += PrintI(Type.CharType, false)
        out += LoadVar(idxTemp)
        out += PushInt(oneVal)
        out += AddI
        out += StoreVar(idxTemp)
        out += Jump(whileLabel)
        out += Label(endLabel)
        if nl then
            out += PushString("")
            out += PrintI(Type.StringType, true)
    }

    // Compiles an rvalue, dispatching on rvalue kind
    private def compileRValue(rVal: TypedRValue, out: ListBuffer[Instr])(using labeller: Labeller): Unit = rVal match {
        case TypedRValue.RExpr(expr) => compileExpr(expr, out)
        case TypedRValue.Newpair(fst, snd, _) =>
            compileExpr(fst, out)
            compileExpr(snd, out)
            out += NewPairI
        case TypedRValue.Fst(lVal, _) => compileValPair(lVal, LoadFstI, out)
        case TypedRValue.Snd(lVal, _) => compileValPair(lVal, LoadSndI, out)
        case TypedRValue.Call(name, args, _) =>
            args.foreach(compileExpr(_, out))
            out += CallI(name, args.length)
        case TypedRValue.ArrayLit(xs, elemTy) =>
            out += PushInt(xs.length)
            out += NewArrayI(elemTy)
            xs.zipWithIndex.foreach { (expr, index) =>
                out += DupI
                out += PushInt(index)
                compileExpr(expr, out)
                out += ArrayStoreI(elemTy)
            }
    }

    // Loads the address of an lvalue onto the stack, traversing nested structures
    private def compileLValueRef(lVal: TypedLValue, out: ListBuffer[Instr])(using labeller: Labeller): Unit = lVal match {
        case TypedLValue.Ident(name) => out += LoadVar(name)
        case TypedLValue.ArrayElem(name, idx, _) =>
            out += LoadVar(name)
            var currentTy: wacc.ast.SemType = name.ty
            idx.foreach { index =>
                currentTy = unwrapOnce(currentTy)
                compileExpr(index, out)
                out += ArrayLoadI(currentTy)
            }
        case TypedLValue.Fst(inner, _) => compileValPair(inner, LoadFstI, out)

        case TypedLValue.Snd(inner, _) => compileValPair(inner, LoadSndI, out)
    }

    // Loads a pair element onto the stack using a given load instruction
    def compileValPair(lVal: TypedLValue, instr: Instr, out: ListBuffer[Instr])(using labeller: Labeller) = {
        compileLValueRef(lVal, out)
        out += instr
    }

    // Compiles short-circuit and, jumping to false label if either operand is false
    private def compileAnd(expr: TypedExpr, other: TypedExpr, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        val falseLabel = labeller.generate()
        val endLabel = labeller.generate()
        compileExpr(expr, out)
        out += JumpIfFalse(falseLabel)
        compileExpr(other, out)
        out += JumpIfFalse(falseLabel)
        out += PushBool(true)
        out += Jump(endLabel)
        out += Label(falseLabel)
        out += PushBool(false)
        out += Label(endLabel)
    }

    // Compiles short-circuit or, jumping past true push if first operand is false
    private def compileOr(expr: TypedExpr, other: TypedExpr, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        val yLabel = labeller.generate()
        val endLabel = labeller.generate()
        compileExpr(expr, out)
        out += JumpIfFalse(yLabel)
        out += PushBool(true)
        out += Jump(endLabel)
        out += Label(yLabel)
        compileExpr(other, out)
        out += Label(endLabel)
    }

    // Compiles an expression to IR by dispatching on expression kind
    private def compileExpr(expr: TypedExpr, out: ListBuffer[Instr])(using labeller: Labeller): Unit = expr match {
        case IntLit(expr)  => out += PushInt(expr)
        case BoolLit(expr) => out += PushBool(expr)
        case CharLit(expr) => out += PushChar(expr)
        case StrLit(expr)  => out += PushString(expr)
        case PairLit()  => out += PushNull

        case Add(expr, other)       => compileTwo(expr, other, AddI, out)
        case Sub(expr, other)       => compileTwo(expr, other, SubI, out)
        case Mul(expr, other)       => compileTwo(expr, other, MulI, out)
        case Div(expr, other)       => compileTwo(expr, other, DivI, out)
        case Mod(expr, other)       => compileTwo(expr, other, ModI, out)

        case Equals(expr, other)    => compileTwo(expr, other, EqI, out)
        case NotEquals(expr, other) => compileTwo(expr, other, NeI, out)
        case Less(expr, other)      => compileTwo(expr, other, LtI, out)
        case LessEq(expr, other)    => compileTwo(expr, other, LeI, out)
        case Greater(expr, other)   => compileTwo(expr, other, GtI, out)
        case GreaterEq(expr, other) => compileTwo(expr, other, GeI, out)

        case Not(expr) => compileOne(expr, NotI, out)
        case Neg(expr) => compileOne(expr, NegI, out)
        case Len(expr) => compileOne(expr, LenI, out)
        case Ord(expr) => compileOne(expr, OrdI, out)
        case Chr(expr) => compileOne(expr, ChrI, out)

        case And(expr, other) => compileAnd(expr, other, out)
        case Or(expr, other)  => compileOr(expr, other, out)

        case Ident(name) => out += LoadVar(name)
        case ArrayElem(name, idx, _) =>
            out += LoadVar(name)
            var currentTy: wacc.ast.SemType = name.ty
            idx.foreach { index =>
                currentTy = unwrapOnce(currentTy)
                compileExpr(index, out)
                out += ArrayLoadI(currentTy)
            }
    }

    // Compiles a binary operation by evaluating both operands then emitting op
    private def compileTwo(expr: TypedExpr, other: TypedExpr, op: Instr, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        compileExpr(expr, out)
        compileExpr(other, out)
        out += op
    }

    // Compiles a unary operation by evaluating the operand then emitting op
    private def compileOne(expr: TypedExpr, op: Instr, out: ListBuffer[Instr])(using labeller: Labeller): Unit = {
        compileExpr(expr, out)
        out += op
    }
}

class Labeller:
    private var counter = zeroCount

    // Generates a fresh unique integer label
    def generate(): Int = {
        val initial = counter
        counter += incrementCount
        initial
    }