package wacc

import ast._
import renamer.Renamed
import scala.collection.mutable

enum Constraint {
    case Is(refTy: SemType)
    case IsOrdered
}

object Constraint {
    val Infer = Is(?)
    val isArray = Is(Type.ArrayType(?))
    val isPair = Is(Type.Pair)
}

sealed trait Error {
    def toSemanticError(fileName: String): SemanticError
}

object Error {
    case class TypeMismatch(expected: SemType, got: SemType, pos: Pos) extends Error {
        def toSemanticError(fileName: String): SemanticError =
            SemanticError(fileName, pos, ErrorCategory.Type, s"Type mismatch: expected $expected, got $got")
    }
    
    case class UndefinedFunction(name: String, pos: Pos) extends Error {
        def toSemanticError(fileName: String): SemanticError =
            SemanticError(fileName, pos, ErrorCategory.Type, s"Function $name is not defined")
    }
    
    case class RedefinedFunction(name: String, pos: Pos) extends Error {
        def toSemanticError(fileName: String): SemanticError =
            SemanticError(fileName, pos, ErrorCategory.Type, s"Function $name is already defined")
    }
    
    case class WrongArgCount(expected: Int, got: Int, pos: Pos) extends Error {
        def toSemanticError(fileName: String): SemanticError =
            SemanticError(fileName, pos, ErrorCategory.Type, s"Wrong number of arguments: expected $expected, got $got")
    }
    
    case class ReturnOutsideFunction(pos: Pos) extends Error {
        def toSemanticError(fileName: String): SemanticError =
            SemanticError(fileName, pos, ErrorCategory.Type, "Return statement outside function")
    }
    
    case class CannotInferType(pos: Pos) extends Error {
        def toSemanticError(fileName: String): SemanticError =
            SemanticError(fileName, pos, ErrorCategory.Type, "Cannot infer type")
    }
    
    case class CannotFreeNonHeap(ty: SemType, pos: Pos) extends Error {
        def toSemanticError(fileName: String): SemanticError =
            SemanticError(fileName, pos, ErrorCategory.Type, s"Cannot free non-heap type $ty")
    }
    
    def toSemanticErrors(errors: List[Error], fileName: String): List[SemanticError] =
        errors.map(_.toSemanticError(fileName))
}

case class FuncSig(returnType: Type, params: List[Type])

class TCState(errs: mutable.Builder[Error, List[Error]], 
              val funcTable: mutable.Map[String, FuncSig], 
              var inFunc: Boolean = false, 
              var funcReturnType: Option[Type] = None) {
    def emit(err: Error): None.type = { errs += err; None }
    def errors: List[Error] = errs.result()
    def enterFunc(retTy: Type): Unit = { inFunc = true; funcReturnType = Some(retTy) }
    def exitFunc(): Unit = { inFunc = false; funcReturnType = None }
}

def unifyPairElem(e1: PairElemType, e2: PairElemType): Option[PairElemType] = (e1, e2) match {
  case (PairElemType.Elem(t1), PairElemType.Elem(t2)) =>
    (t1 === t2) match {
      case Some(t: Type) => Some(PairElemType.Elem(t))
      case _ => None
    }
  case (PairElemType.ErasedPair, PairElemType.ErasedPair) => Some(PairElemType.ErasedPair)
  case (PairElemType.ErasedPair, PairElemType.Elem(p: Type.PairType)) => Some(PairElemType.Elem(p))
  case (PairElemType.Elem(p: Type.PairType), PairElemType.ErasedPair) => Some(PairElemType.Elem(p))
  case (PairElemType.ErasedPair, PairElemType.Elem(Type.Pair)) => Some(PairElemType.ErasedPair)
  case (PairElemType.Elem(Type.Pair), PairElemType.ErasedPair) => Some(PairElemType.ErasedPair)
  case _ => None
}

extension (ty1: SemType) {
    def ===(ty2: SemType): Option[SemType] = (ty1, ty2) match {
        case (?, ty) => Some(ty)
        case (ty, ?) => Some(ty)
        case (t1, t2) if t1 == t2 => Some(t1)
        case (Type.ArrayType(e1), Type.ArrayType(e2)) => (e1 === e2).map(Type.ArrayType(_))
        case (Type.PairType(f1, s1), Type.PairType(f2, s2)) =>
            for { f <- unifyPairElem(f1, f2); s <- unifyPairElem(s1, s2) }
            yield Type.PairType(f, s)
        case (Type.Pair, p: Type.PairType) => Some(p)
        case (p: Type.PairType, Type.Pair) => Some(p)
        case _ => None
    }

    def ~(ty2: SemType): Option[SemType] = (ty1, ty2) match {
        case (Type.ArrayType(Type.CharType), Type.StringType) => Some(Type.StringType)
        case _ => ty1 === ty2
    }
}

extension (ty: SemType) {
    def satisfies(c: Constraint, pos: Pos)(using st: TCState): Option[SemType] = (ty, c) match {
        case (kty, Constraint.Is(refTy)) => 
            (kty ~ refTy).orElse(st.emit(Error.TypeMismatch(refTy, kty, pos)))
        case (?, _) => Some(?)
        case (Type.IntType | Type.CharType, Constraint.IsOrdered) => Some(ty)
        case (ty, Constraint.IsOrdered) => st.emit(Error.TypeMismatch(Type.IntType, ty, pos))
    }
}

sealed trait TypedExpr {
    def ty: SemType
}
object TypedExpr {
    case class IntLit(x: Int) extends TypedExpr { def ty = Type.IntType }
    case class BoolLit(x: Boolean) extends TypedExpr { def ty = Type.BoolType }
    case class CharLit(x: Char) extends TypedExpr { def ty = Type.CharType }
    case class StrLit(x: String) extends TypedExpr { def ty = Type.StringType }
    case class PairLit() extends TypedExpr { def ty = Type.Pair }
    case class Add(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Sub(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Mul(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Div(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Mod(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Ident(x: Renamed) extends TypedExpr { def ty = x.ty }
    case class ArrayElem(arr: Renamed, indices: List[TypedExpr], ty: SemType) extends TypedExpr
    case class Not(x: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class Neg(x: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Len(x: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Ord(x: TypedExpr) extends TypedExpr { def ty = Type.IntType }
    case class Chr(x: TypedExpr) extends TypedExpr { def ty = Type.CharType }
    case class Greater(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class GreaterEq(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class Less(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class LessEq(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class Equals(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class NotEquals(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class And(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
    case class Or(x: TypedExpr, y: TypedExpr) extends TypedExpr { def ty = Type.BoolType }
}

sealed trait TypedStmt
object TypedStmt {
    case object Skip extends TypedStmt
    case class Seq(stmts: List[TypedStmt]) extends TypedStmt
    case class Begin(stmt: TypedStmt) extends TypedStmt
    case class Print(x: TypedExpr, newline: Boolean) extends TypedStmt
    case class Exit(x: TypedExpr) extends TypedStmt
    case class If(cond: TypedExpr, thenS: TypedStmt, elseS: TypedStmt) extends TypedStmt
    case class While(cond: TypedExpr, body: TypedStmt) extends TypedStmt
    case class Read(lval: TypedLValue) extends TypedStmt
    case class Declare(ty: Type, name: Renamed, rval: TypedRValue) extends TypedStmt
    case class Assign(lval: TypedLValue, rval: TypedRValue) extends TypedStmt
    case class Free(x: TypedExpr) extends TypedStmt
    case class Return(x: TypedExpr) extends TypedStmt
}

sealed trait TypedLValue {
    def ty: SemType
}
object TypedLValue {
    case class Ident(x: Renamed) extends TypedLValue { def ty = x.ty }
    case class Fst(x: TypedLValue, ty: SemType) extends TypedLValue
    case class Snd(x: TypedLValue, ty: SemType) extends TypedLValue
    case class ArrayElem(arr: Renamed, indices: List[TypedExpr], ty: SemType) extends TypedLValue
}

sealed trait TypedRValue {
    def ty: SemType
}
object TypedRValue {
    case class RExpr(x: TypedExpr) extends TypedRValue { def ty = x.ty }
    case class ArrayLit(elems: List[TypedExpr], ty: SemType) extends TypedRValue
    case class Newpair(fst: TypedExpr, snd: TypedExpr, ty: SemType) extends TypedRValue
    case class Fst(x: TypedLValue, ty: SemType) extends TypedRValue
    case class Snd(x: TypedLValue, ty: SemType) extends TypedRValue
    case class Call(name: String, args: List[TypedExpr], ty: SemType) extends TypedRValue
}

case class TypedFunc(name: String, params: List[Renamed], body: TypedStmt)
case class TypedProgram(funcs: List[TypedFunc], body: TypedStmt)

object typeChecker {
    def check(program: Program[Renamed]): (TypedProgram, List[Error]) = {
        val funcTable = mutable.Map.empty[String, FuncSig]
        given st: TCState = TCState(List.newBuilder[Error], funcTable)
        
        for (f <- program.fs) {
            if (funcTable.contains(f.name)) {
                st.emit(Error.RedefinedFunction(f.name, f.pos))
            } else {
                funcTable(f.name) = FuncSig(f.returnType, f.params.map(_.t))
            }
        }
        
        val typedFuncs = program.fs.map(checkFunc)
        val typedBody = checkStmt(program.body)
        (TypedProgram(typedFuncs, typedBody), st.errors)
    }

    def checkFunc(f: Func[Renamed])(using st: TCState): TypedFunc = {
        st.enterFunc(f.returnType)
        val typedBody = checkStmt(f.body)
        st.exitFunc()
        TypedFunc(f.name, f.params.map(_.name), typedBody)
    }

    def checkExpr(expr: Expr[Renamed], c: Constraint)(using TCState): (TypedExpr, Option[SemType]) = 
        expr match {
            case IntLit(x, pos)  => (TypedExpr.IntLit(x), Type.IntType.satisfies(c, pos))
            case BoolLit(x, pos) => (TypedExpr.BoolLit(x), Type.BoolType.satisfies(c, pos))
            case CharLit(x, pos) => (TypedExpr.CharLit(x), Type.CharType.satisfies(c, pos))
            case StrLit(x, pos)  => (TypedExpr.StrLit(x), Type.StringType.satisfies(c, pos))
            case PairLit(_, pos) => (TypedExpr.PairLit(), Type.Pair.satisfies(c, pos))
            case Ident(renamed, pos) => 
                val ty = renamed.ty.satisfies(c, pos)
                (TypedExpr.Ident(renamed), ty)
            case ArrayElem(Ident(arr, _), indices, pos) =>
                val typedIdx = indices.map(i => checkExpr(i, Constraint.Is(Type.IntType))._1)
                val elemTy = unwrapArray(arr.ty, indices.length, pos)
                (TypedExpr.ArrayElem(arr, typedIdx, elemTy), elemTy.satisfies(c, pos))
            case Not(x, pos) =>
                val (typed, _) = checkExpr(x, Constraint.Is(Type.BoolType))
                (TypedExpr.Not(typed), Type.BoolType.satisfies(c, pos))
            case Neg(x, pos) =>
                val (typed, _) = checkExpr(x, Constraint.Is(Type.IntType))
                (TypedExpr.Neg(typed), Type.IntType.satisfies(c, pos))
            case Len(x, pos) =>
                val (typed, _) = checkExpr(x, Constraint.isArray)
                (TypedExpr.Len(typed), Type.IntType.satisfies(c, pos))
            case Ord(x, pos) =>
                val (typed, _) = checkExpr(x, Constraint.Is(Type.CharType))
                (TypedExpr.Ord(typed), Type.IntType.satisfies(c, pos))
            case Chr(x, pos) =>
                val (typed, _) = checkExpr(x, Constraint.Is(Type.IntType))
                (TypedExpr.Chr(typed), Type.CharType.satisfies(c, pos))            
            case Add(x, y, pos) => checkIntBinOp(x, y, c, pos, TypedExpr.Add.apply)
            case Sub(x, y, pos) => checkIntBinOp(x, y, c, pos, TypedExpr.Sub.apply)
            case Mul(x, y, pos) => checkIntBinOp(x, y, c, pos, TypedExpr.Mul.apply)
            case Div(x, y, pos) => checkIntBinOp(x, y, c, pos, TypedExpr.Div.apply)
            case Mod(x, y, pos) => checkIntBinOp(x, y, c, pos, TypedExpr.Mod.apply)
            case Greater(x, y, pos) => checkOrdBinOp(x, y, c, pos, TypedExpr.Greater.apply)
            case GreaterEq(x, y, pos) => checkOrdBinOp(x, y, c, pos, TypedExpr.GreaterEq.apply)
            case Less(x, y, pos) => checkOrdBinOp(x, y, c, pos, TypedExpr.Less.apply)
            case LessEq(x, y, pos) => checkOrdBinOp(x, y, c, pos, TypedExpr.LessEq.apply)
            case Equals(x, y, pos) => checkEqBinOp(x, y, c, pos, TypedExpr.Equals.apply)
            case NotEquals(x, y, pos) => checkEqBinOp(x, y, c, pos, TypedExpr.NotEquals.apply)
            case And(x, y, pos) => checkBoolBinOp(x, y, c, pos, TypedExpr.And.apply)
            case Or(x, y, pos) => checkBoolBinOp(x, y, c, pos, TypedExpr.Or.apply)
        }
    
    def checkStmt(stmt: Stmt[Renamed])(using st: TCState): TypedStmt = stmt match {
        case Stmt.Skip => TypedStmt.Skip
        case Stmt.Seq(stmts) => TypedStmt.Seq(stmts.map(checkStmt))
        case Stmt.Begin(s, _) => TypedStmt.Begin(checkStmt(s))        
        case Stmt.Print(e, nl, _) =>
            val (typed, _) = checkExpr(e, Constraint.Infer)
            TypedStmt.Print(typed, nl)
        case Stmt.Exit(e, _) =>
            val (typed, _) = checkExpr(e, Constraint.Is(Type.IntType))
            TypedStmt.Exit(typed)
        case Stmt.If(c, t, e, _) =>
            val (cTyped, _) = checkExpr(c, Constraint.Is(Type.BoolType))
            TypedStmt.If(cTyped, checkStmt(t), checkStmt(e))
        case Stmt.While(c, s, _) =>
            val (cTyped, _) = checkExpr(c, Constraint.Is(Type.BoolType))
            TypedStmt.While(cTyped, checkStmt(s))
        case Stmt.Read(lv, pos) =>
            val (typed, ty) = checkLValue(lv, Constraint.IsOrdered)
            ty match {
                case Some(?) => st.emit(Error.CannotInferType(pos))
                case _ => ()
            }
            TypedStmt.Read(typed)   
        case Stmt.Declare(ty, name, rv, _) =>
            val (rTyped, _) = checkRValue(rv, Constraint.Is(ty))
            TypedStmt.Declare(ty, name, rTyped)
        case Stmt.Assign(lv, rv, _) =>
            val (lTyped, lTy) = checkLValue(lv, Constraint.Infer)
            val c = lTy.fold(Constraint.Infer)(Constraint.Is(_))
            val (rTyped, _) = checkRValue(rv, c)
            TypedStmt.Assign(lTyped, rTyped)
        case Stmt.Free(e, pos) =>
            val (typed, ty) = checkExpr(e, Constraint.Infer)
            ty match {
                case Some(t) => t match {
                    case Type.ArrayType(_) | Type.PairType(_, _) | Type.Pair => ()
                    case _ => st.emit(Error.CannotFreeNonHeap(t, pos))
                }
                case None => ()
            }
            TypedStmt.Free(typed)
        case Stmt.Return(e, pos) =>
            if (!st.inFunc) {
                st.emit(Error.ReturnOutsideFunction(pos))
            }
            val c = st.funcReturnType.fold(Constraint.Infer)(Constraint.Is(_))
            val (typed, _) = checkExpr(e, c)
            TypedStmt.Return(typed)
    }

    def checkLValue(lv: LValue[Renamed], c: Constraint)(using TCState): (TypedLValue, Option[SemType]) = 
        lv match {
            case Ident(r, pos) =>
                val ty = r.ty.satisfies(c, pos)
                (TypedLValue.Ident(r), ty)
            case LValue.Fst(lv, pos) =>
                val (inner, innerTy) = checkLValue(lv, Constraint.isPair)
                val fstTy = extractFst(innerTy, pos)
                (TypedLValue.Fst(inner, fstTy), fstTy.satisfies(c, pos))
            case LValue.Snd(lv, pos) =>
                val (inner, innerTy) = checkLValue(lv, Constraint.isPair)
                val sndTy = extractSnd(innerTy, pos)
                (TypedLValue.Snd(inner, sndTy), sndTy.satisfies(c, pos))
            case ArrayElem(Ident(arr, _), indices, pos) =>
                val typedIdx = indices.map(i => checkExpr(i, Constraint.Is(Type.IntType))._1)
                val elemTy = unwrapArray(arr.ty, indices.length, pos)
                (TypedLValue.ArrayElem(arr, typedIdx, elemTy), elemTy.satisfies(c, pos))
        }
    
    def checkRValue(rv: RValue[Renamed], c: Constraint)(using st: TCState): (TypedRValue, Option[SemType]) = 
        rv match {
            case RValue.RExpr(e, pos) =>
                val (typed, ty) = checkExpr(e, c)
                (TypedRValue.RExpr(typed), ty)
            case RValue.ArrayLit(elems, pos) =>
                val typed = elems.map(e => checkExpr(e, Constraint.Infer)._1)
                val elemTy = typed.foldLeft[Option[SemType]](Some(?)) { (acc, e) =>
                    acc.flatMap(_ ~ e.ty)
                }
                val arrTy = elemTy.map(Type.ArrayType(_)).getOrElse(Type.ArrayType(?))
                val resolvedElemTy = elemTy.getOrElse(?)
                elemTy match {
                    case None => st.emit(Error.CannotInferType(pos))
                    case _ => ()
                }
                (TypedRValue.ArrayLit(typed, resolvedElemTy), arrTy.satisfies(c, pos))
            case RValue.Newpair(x, y, pos) =>
                val (xT, xTy) = checkExpr(x, Constraint.Infer)
                val (yT, yTy) = checkExpr(y, Constraint.Infer)
                val pairTy = Type.PairType(
                    toPairElem(xTy), 
                    toPairElem(yTy)
                )
                (TypedRValue.Newpair(xT, yT, pairTy), pairTy.satisfies(c, pos))            
            case RValue.Fst(lv, pos) =>
                val (inner, innerTy) = checkLValue(lv, Constraint.isPair)
                val fstTy = extractFst(innerTy, pos)
                if (fstTy == ? && c == Constraint.Infer) {
                    st.emit(Error.CannotInferType(pos))
                }
                (TypedRValue.Fst(inner, fstTy), fstTy.satisfies(c, pos))
            case RValue.Snd(lv, pos) =>
                val (inner, innerTy) = checkLValue(lv, Constraint.isPair)
                val sndTy = extractSnd(innerTy, pos)
                if (sndTy == ? && c == Constraint.Infer) {
                    st.emit(Error.CannotInferType(pos))
                }
                (TypedRValue.Snd(inner, sndTy), sndTy.satisfies(c, pos))
            case RValue.Call(name, args, pos) =>
                st.funcTable.get(name) match {
                    case None =>
                        st.emit(Error.UndefinedFunction(name, pos))
                        val typedArgs = args.map(a => checkExpr(a, Constraint.Infer)._1)
                        (TypedRValue.Call(name, typedArgs, ?), ?.satisfies(c, pos))
                    case Some(FuncSig(retTy, paramTys)) =>
                        if (args.length != paramTys.length) {
                            st.emit(Error.WrongArgCount(paramTys.length, args.length, pos))
                        }
                        val typedArgs = args.zip(paramTys).map { (a, pTy) =>
                            checkExpr(a, Constraint.Is(pTy))._1
                        }
                        (TypedRValue.Call(name, typedArgs, retTy), retTy.satisfies(c, pos))
                }
        }

    private def checkIntBinOp(x: Expr[Renamed], y: Expr[Renamed], c: Constraint, pos: Pos,
                              build: (TypedExpr, TypedExpr) => TypedExpr)(using TCState) = {
        val (xTyped, _) = checkExpr(x, Constraint.Is(Type.IntType))
        val (yTyped, _) = checkExpr(y, Constraint.Is(Type.IntType))
        (build(xTyped, yTyped), Type.IntType.satisfies(c, pos))
    }

    private def toPairElem(ty: Option[SemType]): PairElemType = ty match {
        case Some(t: Type) => PairElemType.Elem(t)
        case _ => PairElemType.ErasedPair
    }

    private def extractFst(ty: Option[SemType], pos: Pos)(using st: TCState): SemType = ty match {
        case Some(Type.PairType(PairElemType.Elem(t), _)) => t
        case Some(Type.PairType(PairElemType.ErasedPair, _)) => Type.Pair
        case Some(Type.Pair) => ?
        case None => ?
        case Some(t) =>
            st.emit(Error.TypeMismatch(Type.Pair, t, pos))
            ?
    }

    private def extractSnd(ty: Option[SemType], pos: Pos)(using st: TCState): SemType = ty match {
        case Some(Type.PairType(_, PairElemType.Elem(t))) => t
        case Some(Type.PairType(_, PairElemType.ErasedPair)) => Type.Pair
        case Some(Type.Pair) => ?
        case None => ?
        case Some(t) =>
            st.emit(Error.TypeMismatch(Type.Pair, t, pos))
            ?
    }

    private def unwrapArray(ty: SemType, depth: Int, pos: Pos)(using st: TCState): SemType = (ty, depth) match {
        case (_, 0) => ty
        case (Type.ArrayType(inner), n) => unwrapArray(inner, n - 1, pos)
        case (?, _) => ?
        case _ => 
            st.emit(Error.TypeMismatch(Type.ArrayType(?), ty, pos))
            ?
    }

    private def checkOrdBinOp(x: Expr[Renamed], y: Expr[Renamed], c: Constraint, pos: Pos,
                          build: (TypedExpr, TypedExpr) => TypedExpr)(using TCState) = {
        val (xTyped, xTy) = checkExpr(x, Constraint.IsOrdered)
        val (yTyped, _) = checkExpr(y, xTy.fold(Constraint.Infer)(Constraint.Is(_)))
        (build(xTyped, yTyped), Type.BoolType.satisfies(c, pos))
    }

    private def checkEqBinOp(x: Expr[Renamed], y: Expr[Renamed], c: Constraint, pos: Pos,
                         build: (TypedExpr, TypedExpr) => TypedExpr)(using TCState) = {
        val (xTyped, xTy) = checkExpr(x, Constraint.Infer)
        val (yTyped, _) = checkExpr(y, xTy.fold(Constraint.Infer)(Constraint.Is(_)))
        (build(xTyped, yTyped), Type.BoolType.satisfies(c, pos))
    }

    private def checkBoolBinOp(x: Expr[Renamed], y: Expr[Renamed], c: Constraint, pos: Pos,
                           build: (TypedExpr, TypedExpr) => TypedExpr)(using TCState) = {
        val (xTyped, _) = checkExpr(x, Constraint.Is(Type.BoolType))
        val (yTyped, _) = checkExpr(y, Constraint.Is(Type.BoolType))
        (build(xTyped, yTyped), Type.BoolType.satisfies(c, pos))
    }
}