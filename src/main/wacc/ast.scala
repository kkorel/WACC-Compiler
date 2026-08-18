package wacc

import parsley.templates.*
import wacc.Pos

object ast {
    case class Param[N](t: Type, name: N, pos: Pos)
    case class Func[N](returnType: Type, name: String, params: List[Param[N]], body: Stmt[N], pos: Pos)
    case class Program[N](fs: List[Func[N]], body: Stmt[N], pos: Pos)

    object ParamBridge extends PureParserBridge3[Type, String, Pos, Param[String]] {
        def apply(t: Type, n: String, pos: Pos): Param[String] = Param(t, n, pos)
    }
    object FuncBridge extends PureParserBridge5[Type, String, List[Param[String]], Stmt[String], Pos, Func[String]] {
        def apply(rt: Type, f: String, ps: List[Param[String]], body: Stmt[String], pos: Pos): Func[String] = Func(rt, f, ps, body, pos)
    }
    object ProgramBridge extends PureParserBridge3[List[Func[String]], Stmt[String], Pos, Program[String]] {
        def apply(fs: List[Func[String]], body: Stmt[String], pos: Pos): Program[String] = Program(fs, body, pos)
    }

    sealed trait Expr[+N]

    sealed trait Atom[+N] extends Expr[N]
    case class IntLit(x: Int, pos: Pos) extends Atom[Nothing]
    case class BoolLit(x: Boolean, pos: Pos) extends Atom[Nothing] 
    case class CharLit(x: Char, pos: Pos) extends Atom[Nothing]
    case class StrLit(x: String, pos: Pos) extends Atom[Nothing]
    case class PairLit(x: Null, pos: Pos) extends Atom[Nothing] 
    case class Ident[N](x: N, pos: Pos) extends Atom[N], LValue[N]
    case class ArrayElem[N](x: Ident[N], y: List[Expr[N]], pos: Pos) extends Atom[N], LValue[N]

    sealed trait UnaryOp[+N] extends Expr[N]
    case class Not[N](x: Expr[N], pos: Pos) extends UnaryOp[N]
    case class Neg[N](x: Expr[N], pos: Pos) extends UnaryOp[N]
    case class Len[N](x: Expr[N], pos: Pos) extends UnaryOp[N]
    case class Ord[N](x: Expr[N], pos: Pos) extends UnaryOp[N]
    case class Chr[N](x: Expr[N], pos: Pos) extends UnaryOp[N]

    sealed trait BinaryOp[+N] extends Expr[N]
    case class Mul[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Div[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Mod[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Add[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Sub[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Greater[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class GreaterEq[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Less[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class LessEq[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Equals[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class NotEquals[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class And[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]
    case class Or[N](x: Expr[N], y: Expr[N], pos: Pos) extends BinaryOp[N]

    object IntLit extends PureParserBridge2[Int, Pos, IntLit] {
        def apply(x: Int, pos: Pos): IntLit = new IntLit(x, pos)
    }
    object BoolLit extends PureParserBridge2[Boolean, Pos, BoolLit] {
        def apply(x: Boolean, pos: Pos): BoolLit = new BoolLit(x, pos)
    }
    object CharLit extends PureParserBridge2[Char, Pos, CharLit] {
        def apply(x: Char, pos: Pos): CharLit = new CharLit(x, pos)
    }
    object StrLit extends PureParserBridge2[String, Pos, StrLit] {
        def apply(x: String, pos: Pos): StrLit = new StrLit(x, pos)
    }
    object PairLit extends PureParserBridge2[Null, Pos, PairLit] {
        def apply(x: Null, pos: Pos): PairLit = new PairLit(x, pos)
    }
    object IdentBridge extends PureParserBridge2[String, Pos, Ident[String]] {
        def apply(x: String, pos: Pos): Ident[String] = Ident(x, pos)
    }
    object ArrayElemBridge extends PureParserBridge3[Ident[String], List[Expr[String]], Pos, ArrayElem[String]] {
        def apply(x: Ident[String], y: List[Expr[String]], pos: Pos): ArrayElem[String] = ArrayElem(x, y, pos)
    }

    object NotBridge extends PureParserBridge2[Expr[String], Pos, Not[String]] {
         def apply(x: Expr[String], pos: Pos): Not[String] = Not(x, pos)
    }
    object NegBridge extends PureParserBridge2[Expr[String], Pos, Neg[String]] {
         def apply(x: Expr[String], pos: Pos): Neg[String] = Neg(x, pos)
    }
    object LenBridge extends PureParserBridge2[Expr[String], Pos, Len[String]] {
         def apply(x: Expr[String], pos: Pos): Len[String] = Len(x, pos)
    }
    object OrdBridge extends PureParserBridge2[Expr[String], Pos, Ord[String]] {
         def apply(x: Expr[String], pos: Pos): Ord[String] = Ord(x, pos)
    }
    object ChrBridge extends PureParserBridge2[Expr[String], Pos, Chr[String]] {
         def apply(x: Expr[String], pos: Pos): Chr[String] = Chr(x, pos)
    }

    object MulBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Mul[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Mul[String] = Mul(x, y, pos)
    }
    object DivBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Div[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Div[String] = Div(x, y, pos)
    }
    object ModBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Mod[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Mod[String] = Mod(x, y, pos)
    }
    object AddBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Add[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Add[String] = Add(x, y, pos)
    }
    object SubBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Sub[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Sub[String] = Sub(x, y, pos)
    }
    object GreaterBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Greater[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Greater[String] = Greater(x, y, pos)
    }
    object GreaterEqBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, GreaterEq[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): GreaterEq[String] = GreaterEq(x, y, pos)
    }
    object LessBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Less[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Less[String] = Less(x, y, pos)
    }
    object LessEqBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, LessEq[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): LessEq[String] = LessEq(x, y, pos)
    }
    object EqualsBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Equals[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Equals[String] = Equals(x, y, pos)
    }
    object NotEqualsBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, NotEquals[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): NotEquals[String] = NotEquals(x, y, pos)
    }
    object AndBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, And[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): And[String] = And(x, y, pos)
    }
    object OrBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, Or[String]] {
         def apply(x: Expr[String], y: Expr[String], pos: Pos): Or[String] = Or(x, y, pos)
    }

    sealed trait Stmt[+N]
    object Stmt {
        case object Skip extends Stmt[Nothing]
        case class Seq[N](stmts: List[Stmt[N]]) extends Stmt[N]
        case class Declare[N](t: Type, n: N, rhs: RValue[N], pos: Pos) extends Stmt[N]
        case class Assign[N](lhs: LValue[N], rhs: RValue[N], pos: Pos) extends Stmt[N]
        case class Read[N](x: LValue[N], pos: Pos) extends Stmt[N]
        case class Free[N](x: Expr[N], pos: Pos) extends Stmt[N]
        case class Return[N](x: Expr[N], pos: Pos) extends Stmt[N]
        case class Exit[N](x: Expr[N], pos: Pos) extends Stmt[N]
        case class Print[N](x: Expr[N], newline: Boolean, pos: Pos) extends Stmt[N]
        case class If[N](cond: Expr[N], thenx: Stmt[N], elsey: Stmt[N], pos: Pos) extends Stmt[N]
        case class While[N](cond: Expr[N], body: Stmt[N], pos: Pos) extends Stmt[N]
        case class Begin[N](s: Stmt[N], pos: Pos) extends Stmt[N]
    }

    object SeqBridge extends PureParserBridge1[List[Stmt[String]], Stmt.Seq[String]] {
         def apply(stmts: List[Stmt[String]]): Stmt.Seq[String] = Stmt.Seq(stmts)
    }
    object DeclareBridge extends PureParserBridge4[Type, String, RValue[String], Pos, Stmt.Declare[String]] {
         def apply(t: Type, n: String, rhs: RValue[String], pos: Pos): Stmt.Declare[String] = Stmt.Declare(t, n, rhs, pos)
    }
    object AssignBridge extends PureParserBridge3[LValue[String], RValue[String], Pos, Stmt.Assign[String]] {
         def apply(lhs: LValue[String], rhs: RValue[String], pos: Pos): Stmt.Assign[String] = Stmt.Assign(lhs, rhs, pos)
    }
    object ReadBridge extends PureParserBridge2[LValue[String], Pos, Stmt.Read[String]] {
        def apply(x: LValue[String], pos: Pos): Stmt.Read[String] = Stmt.Read(x, pos)
    }
    object FreeBridge extends PureParserBridge2[Expr[String], Pos, Stmt.Free[String]] {
        def apply(x: Expr[String], pos: Pos): Stmt.Free[String] = Stmt.Free(x, pos)
    }
    object ReturnBridge extends PureParserBridge2[Expr[String], Pos, Stmt.Return[String]] {
        def apply(x: Expr[String], pos: Pos): Stmt.Return[String] = Stmt.Return(x, pos)
    }
    object ExitBridge extends PureParserBridge2[Expr[String], Pos, Stmt.Exit[String]] {
        def apply(x: Expr[String], pos: Pos): Stmt.Exit[String] = Stmt.Exit(x, pos)
    }
    object PrintBridge extends PureParserBridge3[Expr[String], Boolean, Pos, Stmt.Print[String]] {
        def apply(x: Expr[String], newline: Boolean, pos: Pos): Stmt.Print[String] = Stmt.Print(x, newline, pos)
    }
    object IfBridge extends PureParserBridge4[Expr[String], Stmt[String], Stmt[String], Pos, Stmt.If[String]] {
        def apply(cond: Expr[String], thenx: Stmt[String], elsey: Stmt[String], pos: Pos): Stmt.If[String] = 
            Stmt.If(cond, thenx, elsey, pos)
    }
    object WhileBridge extends PureParserBridge3[Expr[String], Stmt[String], Pos, Stmt.While[String]] {
        def apply(cond: Expr[String], body: Stmt[String], pos: Pos): Stmt.While[String] = Stmt.While(cond, body, pos)
    }
    object BeginBridge extends PureParserBridge2[Stmt[String], Pos, Stmt.Begin[String]] {
        def apply(s: Stmt[String], pos: Pos): Stmt.Begin[String] = Stmt.Begin(s, pos)
    }

    sealed trait SemType

    sealed trait Type extends SemType
    object Type {
        case object IntType extends Type
        case object BoolType extends Type
        case object CharType extends Type
        case object StringType extends Type
        case object Pair extends Type
        case class ArrayType(t: SemType) extends Type
        case class PairType(fst: PairElemType, snd: PairElemType) extends Type
    }

    sealed trait PairElemType extends SemType
    object PairElemType {
        case class Elem(t: SemType) extends PairElemType
        case object ErasedPair extends PairElemType
    }

    case object ? extends SemType

     object PairTypeBridge extends PureParserBridge2[PairElemType, PairElemType, Type.PairType] {
        def apply(fst: PairElemType, snd: PairElemType): Type.PairType = Type.PairType(fst, snd)
    }
    object PairElemBridge extends PureParserBridge1[Type, PairElemType.Elem] {
        def apply(t: Type): PairElemType.Elem = PairElemType.Elem(t)
    }

    sealed trait LValue[+N]
    object LValue {
        case class Fst[N](x: LValue[N], pos: Pos) extends LValue[N]
        case class Snd[N](y: LValue[N], pos: Pos) extends LValue[N]
    }

    object FstLBridge extends PureParserBridge2[LValue[String], Pos, LValue.Fst[String]] {
        def apply(x: LValue[String], pos: Pos): LValue.Fst[String] = LValue.Fst(x, pos)
    }
    object SndLBridge extends PureParserBridge2[LValue[String], Pos, LValue.Snd[String]] {
        def apply(y: LValue[String], pos: Pos): LValue.Snd[String] = LValue.Snd(y, pos)
    }

    sealed trait RValue[+N]
    object RValue {
        case class RExpr[N](e: Expr[N], pos: Pos) extends RValue[N]
        case class ArrayLit[N](elems: List[Expr[N]], pos: Pos) extends RValue[N]
        case class Newpair[N](x: Expr[N], y: Expr[N], pos: Pos) extends RValue[N]
        case class Fst[N](x: LValue[N], pos: Pos) extends RValue[N]
        case class Snd[N](y: LValue[N], pos: Pos) extends RValue[N]
        case class Call[N](f: String, args: List[Expr[N]], pos: Pos) extends RValue[N]
    }

    object RExprBridge extends PureParserBridge2[Expr[String], Pos, RValue.RExpr[String]] {
        def apply(e: Expr[String], pos: Pos): RValue.RExpr[String] = RValue.RExpr(e, pos)
    }
    object ArrayLitBridge extends PureParserBridge2[List[Expr[String]], Pos, RValue.ArrayLit[String]] {
        def apply(elems: List[Expr[String]], pos: Pos): RValue.ArrayLit[String] = RValue.ArrayLit(elems, pos)
    }
    object NewpairBridge extends PureParserBridge3[Expr[String], Expr[String], Pos, RValue.Newpair[String]] {
        def apply(x: Expr[String], y: Expr[String], pos: Pos): RValue.Newpair[String] = RValue.Newpair(x, y, pos)
    }
    object FstRBridge extends PureParserBridge2[LValue[String], Pos, RValue.Fst[String]] {
        def apply(x: LValue[String], pos: Pos): RValue.Fst[String] = RValue.Fst(x, pos)
    }
    object SndRBridge extends PureParserBridge2[LValue[String], Pos, RValue.Snd[String]] {
        def apply(y: LValue[String], pos: Pos): RValue.Snd[String] = RValue.Snd(y, pos)
    }
    object CallBridge extends PureParserBridge3[String, List[Expr[String]], Pos, RValue.Call[String]] {
        def apply(f: String, args: List[Expr[String]], pos: Pos): RValue.Call[String] = RValue.Call(f, args, pos)
    }

    sealed trait Error
    object Error {
        case class TypeMismatch(got: SemType, expected: SemType) extends Error
    }
}   
