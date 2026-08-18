package wacc

import parsley.errors.combinator.*
import parsley.{Parsley, Result}
import parsley.combinator.*
import parsley.position.pos

import parsley.Parsley.atomic
import parsley.Parsley.{some, many, notFollowedBy}

import lexer.implicits.given
import lexer.{intLit, charLit, stringLit, ident , implicits, fully}
import ast.*
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import wacc.Pos

object parser {   
    private val waccPos: Parsley[Pos] = pos.map { case (l, c) => Pos(l, c) }

    private def unaryOp[A](op: String)(f: (A, Pos) => A): Parsley[A => A] = 
        (pos <* op).map { case (l, c) => (x: A) => f(x, Pos(l, c)) }

    private def binaryOp[A](op: String)(f: (A, A, Pos) => A): Parsley[(A, A) => A] = 
        (pos <* op).map { case (l, c) => (x: A, y: A) => f(x, y, Pos(l, c)) }

    private lazy val boolLit : Parsley[Boolean] = ("true" as true) | ("false" as false) 
    private lazy val pairLit : Parsley[Null] = ("null" as null)

    private lazy val expr: Parsley[Expr[String]] = precedence[Expr[String]](
        IntLit(intLit, waccPos),
        CharLit(charLit, waccPos),
        StrLit(stringLit, waccPos),
        PairLit(pairLit, waccPos),
        BoolLit(boolLit, waccPos),
        atomic(ArrayElemBridge(IdentBridge(ident, waccPos), some("[" ~> expr <~ "]"), waccPos)),
        IdentBridge(ident, waccPos),
        ("(" ~> expr <~ ")").label("expression")
    )(
        Ops(Prefix)(
            unaryOp("!")(NotBridge.apply),
            unaryOp("-")(NegBridge.apply),
            unaryOp("len")(LenBridge.apply),
            unaryOp("ord")(OrdBridge.apply),
            unaryOp("chr")(ChrBridge.apply)
        ),
        Ops(InfixL)(
            binaryOp("*")(MulBridge.apply),
            binaryOp("%")(ModBridge.apply),
            binaryOp("/")(DivBridge.apply)
        ),
        Ops(InfixL)(
            binaryOp("+")(AddBridge.apply),
            binaryOp("-")(SubBridge.apply)
        ),
        Ops(InfixN)(
            binaryOp(">=")(GreaterEqBridge.apply),
            binaryOp(">")(GreaterBridge.apply),
            binaryOp("<=")(LessEqBridge.apply),
            binaryOp("<")(LessBridge.apply)
        ),
        Ops(InfixN)(
            binaryOp("==")(EqualsBridge.apply),
            binaryOp("!=")(NotEqualsBridge.apply)
        ),
        Ops(InfixR)(binaryOp("&&")(AndBridge.apply)),
        Ops(InfixR)(binaryOp("||")(OrBridge.apply))
    ).label("expression")

    private lazy val baseType: Parsley[Type] =
        ("int" ~> Parsley.pure(Type.IntType)) <|>
        ("bool" ~> Parsley.pure(Type.BoolType)) <|>
        ("char" ~> Parsley.pure(Type.CharType)) <|>
        ("string" ~> Parsley.pure(Type.StringType))

    private lazy val coreType: Parsley[Type] =
        atomic(pairType) <|>
        baseType

    private lazy val arrayType: Parsley[Type] =
        (coreType <~> some("[" ~> "]"))
        .map { case (t, arrays) => arrays.foldLeft(t)((x, _) => Type.ArrayType(x)) }

    private lazy val waccType: Parsley[Type] = 
        (atomic(arrayType) <|> coreType).label("type")

    private lazy val pairType: Parsley[Type] =
        PairTypeBridge("pair" ~> ("(" ~> pairElemType) <~ ",", pairElemType <~ ")")

    private lazy val pairElemType: Parsley[PairElemType] = {
        atomic("pair" <~ notFollowedBy("(")) *> Parsley.pure(PairElemType.ErasedPair) <|>
        atomic(PairElemBridge(arrayType)) <|>
        PairElemBridge(baseType)
    }

    private lazy val lvalue: Parsley[LValue[String]] = 
        (atomic(ArrayElemBridge(IdentBridge(ident, waccPos), some("[" ~> expr <~ "]"), waccPos)) <|>
        FstLBridge("fst" ~> lvalue, waccPos) <|>
        SndLBridge("snd" ~> lvalue, waccPos) <|>
        IdentBridge(ident, waccPos)).label("lvalue")

    private lazy val rvalue: Parsley[RValue[String]] =
        (ArrayLitBridge("[" ~> sepBy(expr, ",") <~ "]", waccPos) <|>
        NewpairBridge("newpair" ~> "(" ~> (expr <~ ","), expr <~ ")", waccPos) <|>
        FstRBridge("fst" ~> lvalue, waccPos) <|>
        SndRBridge("snd" ~> lvalue, waccPos) <|>
        CallBridge("call" ~> ident, "(" ~> sepBy(expr, ",") <~ ")", waccPos) <|>
        RExprBridge(expr, waccPos)).label("rvalue")

    private lazy val atomicStatement: Parsley[Stmt[String]] =
        (("skip" as Stmt.Skip) <|>
        DeclareBridge(waccType, ident, "=" ~> rvalue, waccPos) <|>
        AssignBridge(lvalue, "=" ~> rvalue, waccPos) <|>
        ReadBridge("read" ~> lvalue, waccPos) <|>
        FreeBridge("free" ~> expr, waccPos) <|>
        PrintBridge("print" ~> expr, Parsley.pure(false), waccPos) <|>
        PrintBridge("println" ~> expr, Parsley.pure(true), waccPos) <|>
        ReturnBridge("return" ~> expr, waccPos) <|>
        ExitBridge("exit" ~> expr, waccPos) <|>
        IfBridge(
            "if" ~> expr <~ "then",
            stmt <~ "else",
            stmt <~ "fi",
            waccPos
        ) <|>
        WhileBridge(
            "while" ~> expr <~ "do",
            stmt <~ "done",
            waccPos
        ) <|>
        BeginBridge("begin" ~> stmt <~ "end", waccPos)).label("statement")

    private lazy val stmt: Parsley[Stmt[String]] =
        sepBy1(atomicStatement, ";").map { statements => 
            if (statements.lengthCompare(1) == 0) statements.head 
            else SeqBridge(statements) 
        }

    private def returningEnd(s: Stmt[String]): Boolean = s match {
    case Stmt.Return(_, _) | Stmt.Exit(_, _) => true 
    case Stmt.Seq(xs) if xs.nonEmpty => returningEnd(xs.last)
    case Stmt.If(_, thenBlock, elseBlock, _) => returningEnd(thenBlock) && returningEnd(elseBlock)
    case Stmt.Begin(inner, _) => returningEnd(inner) 
    case _ => false
}

    private lazy val param: Parsley[Param[String]] = ParamBridge(waccType, ident, waccPos)

    private lazy val params: Parsley[List[Param[String]]] = 
        sepBy(param, ",")

    private lazy val func: Parsley[Func[String]] = 
        atomic(FuncBridge(waccType, ident, "(" ~> params <~ ")", ("is" ~> stmt <~ "end").filter(returningEnd).explain("function bodies must end with a return or exit statement"), waccPos))

    private lazy val program: Parsley[Program[String]] = 
        ProgramBridge("begin" ~> many(func), stmt <~ "end", waccPos)

    def parseExpr(input: String): Result[String, Expr[String]] = parseExpr.parse(input)
    private val parseExpr = fully(expr)

    def parseStmt(input: String): Result[String, Stmt[String]] = parseStmt.parse(input)
    private val parseStmt = fully(stmt)

    def parseFile(input: String): Result[String, Program[String]] = {
        fully(program).parse(input)
    }

    def parseFile(input: String, filename: String): Result[WACCError, Program[String]] = {
        given errorBuilder: WACCErrorBuilder = new WACCErrorBuilder(filename)
        fully(program).parse(input)(using errorBuilder)
    }
}