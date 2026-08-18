package unit_test

import org.scalatest.flatspec.AnyFlatSpec

import wacc.parser
import wacc.ast.* 
import parsley.{Success, Failure}


class parserTests extends AnyFlatSpec {

    def shouldFail[A](r: parsley.Result[String, A]): Unit = r match {
        case Failure(_) => ()
        case _          => fail("expected parse failure")
    }

    def checkExpr(result: parsley.Result[String, Expr[String]], expected: Expr[String] => Boolean) = {
        result match {
            case Success(expr) if expected(expr) => succeed
            case Success(expr) => fail(s"Structure mismatch: got $expr")
            case Failure(e) => fail(s"Parse failed: $e")
        }
    }

    def checkStmt(result: parsley.Result[String, Stmt[String]], expected: Stmt[String] => Boolean) = {
        result match {
            case Success(stmt) if expected(stmt) => succeed
            case Success(stmt) => fail(s"Structure mismatch: got $stmt")
            case Failure(e) => fail(s"Parse failed: $e")
        }
    }

    def checkProgram(result: parsley.Result[String, Program[String]], expected: Program[String] => Boolean) = {
        result match {
            case Success(prog) if expected(prog) => succeed
            case Success(prog) => fail(s"Structure mismatch: got $prog")
            case Failure(e) => fail(s"Parse failed: $e")
        }
    }

    it should "be able to parse test" in {
        parser.parseFile("""begin int x = "Hello" end""", "test") match {
            case Success(_) => succeed
            case Failure(e) => fail(s"Expected success but got failure: $e")
        }
    }

    it should "be able to parse atoms" in {
        checkExpr(parser.parseExpr("13"), { case IntLit(13, _) => true; case _ => false })
        checkExpr(parser.parseExpr("'x'"), { case CharLit('x', _) => true; case _ => false })
        checkExpr(parser.parseExpr("yo"), { case Ident("yo", _) => true; case _ => false })
        checkExpr(parser.parseExpr("null"), { case PairLit(null, _) => true; case _ => false })
        checkExpr(parser.parseExpr("true"), { case BoolLit(true, _) => true; case _ => false })
        checkExpr(parser.parseExpr("\"hello\""), { case StrLit("hello", _) => true; case _ => false })
        checkExpr(parser.parseExpr("hello[hello]"), { 
            case ArrayElem(Ident("hello", _), List(Ident("hello", _)), _) => true
            case _ => false 
        })
    }

    it should "be able to parse whitespace" in {
        checkExpr(parser.parseExpr("hello ['a'  ]  [b]  [c]"), {
            case ArrayElem(Ident("hello", _), List(CharLit('a', _), Ident("b", _), Ident("c", _)), _) => true
            case _ => false
        })
    }

    it should "parse simple unary expressions" in {
        checkExpr(parser.parseExpr("!true"), { case Not(BoolLit(true, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("-5"), { case IntLit(-5, _) => true; case _ => false })
        checkExpr(parser.parseExpr("len(arr)"), { case Len(Ident("arr", _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("ord 'a'"), { case Ord(CharLit('a', _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("chr 65"), { case Chr(IntLit(65, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("len(ord(-5))"), { case Len(Ord(IntLit(-5, _), _), _) => true; case _ => false })
    }

    it should "parse simple binary expressions" in {
        checkExpr(parser.parseExpr("yo + 2"), { case Add(Ident("yo", _), IntLit(2, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("5 - 3"), { case Sub(IntLit(5, _), IntLit(3, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("4 * 2"), { case Mul(IntLit(4, _), IntLit(2, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("8 / 2"), { case Div(IntLit(8, _), IntLit(2, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("7 % 3"), { case Mod(IntLit(7, _), IntLit(3, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("1 < 2"), { case Less(IntLit(1, _), IntLit(2, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("1 <= 2"), { case LessEq(IntLit(1, _), IntLit(2, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("2 > 1"), { case Greater(IntLit(2, _), IntLit(1, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("2 >= 1"), { case GreaterEq(IntLit(2, _), IntLit(1, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("1 == 1"), { case Equals(IntLit(1, _), IntLit(1, _), _) => true; case _ => false })
        checkExpr(parser.parseExpr("1 != 2"), { case NotEquals(IntLit(1, _), IntLit(2, _), _) => true; case _ => false })
    }

    it should "respect operator precedence" in {
        checkExpr(parser.parseExpr("1 + 2 * 3"), {
            case Add(IntLit(1, _), Mul(IntLit(2, _), IntLit(3, _), _), _) => true
            case _ => false
        })

        checkExpr(parser.parseExpr("4 * 2 + 1"), {
            case Add(Mul(IntLit(4, _), IntLit(2, _), _), IntLit(1, _), _) => true
            case _ => false
        })

        checkExpr(parser.parseExpr("1 < 2 && 3 < 4"), {
            case And(Less(IntLit(1, _), IntLit(2, _), _), Less(IntLit(3, _), IntLit(4, _), _), _) => true
            case _ => false
        })
    }

    it should "respect associativity rules" in {
        checkExpr(parser.parseExpr("1 - 2 - 3"), {
            case Sub(Sub(IntLit(1, _), IntLit(2, _), _), IntLit(3, _), _) => true
            case _ => false
        })

        checkExpr(parser.parseExpr("true || false || true"), {
            case Or(BoolLit(true, _), Or((BoolLit(false, _)), BoolLit(true, _), _), _) => true
            case _ => false
        })

        shouldFail(parser.parseExpr("x < y < z"))
    }

    it should "parse parenthesised expressions" in {
        checkExpr(parser.parseExpr("(1 + 2) * 3"), {
            case Mul(Add(IntLit(1, _), IntLit(2, _), _), IntLit(3, _), _) => true
            case _ => false
        })

        checkExpr(parser.parseExpr("!(1 == 2)"), {
            case Not(Equals(IntLit(1, _), IntLit(2, _), _), _) => true
            case _ => false
        })

        checkExpr(parser.parseExpr("(5+2)"), { case Add(IntLit(5, _), IntLit(2, _), _) => true; case _ => false })
    }

    it should "parse mixed complex expressions" in {
        checkExpr(parser.parseExpr("x + list['a'] * (z - 1)"), {
            case Add(Ident("x", _), Mul(ArrayElem(Ident("list", _), List(CharLit('a', _)), _), Sub(Ident("z", _), IntLit(1, _), _), _), _) => true
            case _ => false
        })

        checkExpr(parser.parseExpr("len arr > 0 && arr[0] == 1"), {
            case And(Greater(Len(Ident("arr", _), _), IntLit(0, _), _), Equals(ArrayElem(Ident("arr", _), List(IntLit(0, _)), _), IntLit(1, _), _), _) => true
            case _ => false
        })
    }

    it should "reject missing operands" in {
        shouldFail(parser.parseExpr("+ 1"))
        shouldFail(parser.parseExpr("1 +"))
        shouldFail(parser.parseExpr("!"))
        shouldFail(parser.parseExpr("len"))
    }
    
    it should "reject mismatched parentheses" in {
        shouldFail(parser.parseExpr("(1 + 2"))
        shouldFail(parser.parseExpr("1 + 2)"))
        shouldFail(parser.parseExpr("((3)"))
    }

    it should "reject invalid atoms" in {
        shouldFail(parser.parseExpr(""))
        shouldFail(parser.parseExpr("()"))
        shouldFail(parser.parseExpr("''"))
        shouldFail(parser.parseExpr("\""))
        shouldFail(parser.parseExpr("\'"))
        shouldFail(parser.parseExpr("\\"))
    }
       
    it should "reject malformed array access" in {
        shouldFail(parser.parseExpr("arr[]"))
        shouldFail(parser.parseExpr("arr["))
        shouldFail(parser.parseExpr("arr]"))
        shouldFail(parser.parseExpr("arr[1 2]"))
    }

    it should "reject keywords used as identifiers" in {
        shouldFail(parser.parseExpr("true = 5"))
        shouldFail(parser.parseExpr("len = 3"))
    }

    it should "parse escaped character literals" in {
        checkExpr(parser.parseExpr("'\\n'"), { case CharLit('\n', _) => true; case _ => false })
        checkExpr(parser.parseExpr("'\\t'"), { case CharLit('\t', _) => true; case _ => false })
        checkExpr(parser.parseExpr("'\\r'"), { case CharLit('\r', _) => true; case _ => false })
        checkExpr(parser.parseExpr("'\\f'"), { case CharLit('\f', _) => true; case _ => false })
        checkExpr(parser.parseExpr("'\\0'"), { case CharLit('\u0000', _) => true; case _ => false })
        checkExpr(parser.parseExpr("'\\\''"), { case CharLit('\'', _) => true; case _ => false })
        checkExpr(parser.parseExpr("'\\\"'"), { case CharLit('\"', _) => true; case _ => false })
        checkExpr(parser.parseExpr("'\\\\'"), { case CharLit('\\', _) => true; case _ => false })
    }

    it should "reject unescaped character literals" in {
        shouldFail(parser.parseExpr("'\\'"))       
        shouldFail(parser.parseExpr("'\"'"))
        shouldFail(parser.parseExpr("'\''"))       
        shouldFail(parser.parseExpr("'\\\\\\'"))   
        shouldFail(parser.parseExpr("'\n'"))    
        shouldFail(parser.parseExpr("'"))         
    }

    it should "parse escaped string literals" in {
        checkExpr(parser.parseExpr("\"hello\\nworld\""), { case StrLit("hello\nworld", _) => true; case _ => false })
        checkExpr(parser.parseExpr("\"tab\\tspace\""), { case StrLit("tab\tspace", _) => true; case _ => false })
        checkExpr(parser.parseExpr("\"quote: \\\"\""), { case StrLit("quote: \"", _) => true; case _ => false })
        checkExpr(parser.parseExpr("\"backslash: \\\\\""), { case StrLit("backslash: \\", _) => true; case _ => false })
    }

    it should "parse all basic statements correctly" in {
        checkStmt(parser.parseStmt("skip"), { case Stmt.Skip => true; case _ => false })

        checkStmt(parser.parseStmt("int p = 11"), { 
            case Stmt.Declare(Type.IntType, "p", RValue.RExpr(IntLit(11, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("v = 67"), { 
            case Stmt.Assign(Ident("v", _), RValue.RExpr(IntLit(67, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("read t"), { case Stmt.Read(Ident("t", _), _) => true; case _ => false })
        checkStmt(parser.parseStmt("read ts[1]"), { case Stmt.Read(ArrayElem(Ident("ts", _), List(IntLit(1, _)), _), _) => true; case _ => false })
        checkStmt(parser.parseStmt("free h"), { case Stmt.Free(Ident("h", _), _) => true; case _ => false })
        checkStmt(parser.parseStmt("free hs[4]"), { case Stmt.Free(ArrayElem(Ident("hs", _), List(IntLit(4, _)), _), _) => true; case _ => false })
        checkStmt(parser.parseStmt("return 0"), { case Stmt.Return(IntLit(0, _), _) => true; case _ => false })
        checkStmt(parser.parseStmt("exit 7"), { case Stmt.Exit(IntLit(7, _), _) => true; case _ => false })
        checkStmt(parser.parseStmt("print 12321"), { case Stmt.Print(IntLit(12321, _), false, _) => true; case _ => false })
        checkStmt(parser.parseStmt("println 387"), { case Stmt.Print(IntLit(387, _), true, _) => true; case _ => false })

        shouldFail(parser.parseStmt("read 7"))
    }  

    it should "parse begin and end blocks with or without sequences correctly" in {
        checkStmt(parser.parseStmt("begin skip end"), { case Stmt.Begin(Stmt.Skip, _) => true; case _ => false })
        checkStmt(parser.parseStmt("begin skip; skip; skip; skip end"), { 
            case Stmt.Begin(Stmt.Seq(List(Stmt.Skip, Stmt.Skip, Stmt.Skip, Stmt.Skip)), _) => true
            case _ => false
        })
    }

    it should "parse the sequencing operator correctly and reject malformed sequencing" in {
        checkStmt(parser.parseStmt("skip; skip"), { case Stmt.Seq(List(Stmt.Skip, Stmt.Skip)) => true; case _ => false })

        shouldFail(parser.parseStmt("skip;"))
        shouldFail(parser.parseStmt("skip;; skip"))
        shouldFail(parser.parseStmt("(skip)"))
        shouldFail(parser.parseStmt("(skip; skip)"))
    }

    it should "parse while disregarding whitespace and comments" in {
        checkStmt(parser.parseStmt("  int  q   =    1  "), {
            case Stmt.Declare(Type.IntType, "q", RValue.RExpr(IntLit(1, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("int b = 2 # commentttt\n; skip"), {
            case Stmt.Seq(List(Stmt.Declare(Type.IntType, "b", RValue.RExpr(IntLit(2, _), _), _), Stmt.Skip)) => true
            case _ => false
        })
    }

    it should "parse if else statements correctly and reject malformed ones" in {
        checkStmt(parser.parseStmt("if true then skip else skip fi"), {
            case Stmt.If(BoolLit(true, _), Stmt.Skip, Stmt.Skip, _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("if false then skip; skip else skip; skip; skip fi"), {
            case Stmt.If(BoolLit(false, _), Stmt.Seq(List(Stmt.Skip, Stmt.Skip)), Stmt.Seq(List(Stmt.Skip, Stmt.Skip, Stmt.Skip)), _) => true
            case _ => false
        })

        shouldFail(parser.parseStmt("if then skip else skip fi"))
        shouldFail(parser.parseStmt("if true then skip fi"))
        shouldFail(parser.parseStmt("if true then skip else skip"))
    }

    it should "parse while do done statements correctly and reject malformed ones" in {
        checkStmt(parser.parseStmt("while true do skip done"), {
            case Stmt.While(BoolLit(true, _), Stmt.Skip, _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("while false do skip; skip done"), {
            case Stmt.While(BoolLit(false, _), Stmt.Seq(List(Stmt.Skip, Stmt.Skip)), _) => true
            case _ => false
        })

        shouldFail(parser.parseStmt("while do skip done"))
        shouldFail(parser.parseStmt("while true skip done"))
        shouldFail(parser.parseStmt("while true do done"))
        shouldFail(parser.parseStmt("while true do skip"))
    }

    it should "parse both base and nested versions of arrays and pair lvalues correctly" in {
        checkStmt(parser.parseStmt("xs[0] = 17"), {
            case Stmt.Assign(ArrayElem(Ident("xs", _), List(IntLit(0, _)), _), RValue.RExpr(IntLit(17, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("vs[0][3][7] = 9"), {
            case Stmt.Assign(ArrayElem(Ident("vs", _), List(IntLit(0, _), IntLit(3, _), IntLit(7, _)), _), RValue.RExpr(IntLit(9, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("fst m = 90"), {
            case Stmt.Assign(LValue.Fst(Ident("m", _), _), RValue.RExpr(IntLit(90, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("snd k = 60"), {
            case Stmt.Assign(LValue.Snd(Ident("k", _), _), RValue.RExpr(IntLit(60, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("snd fst snd p = 4"), {
            case Stmt.Assign(LValue.Snd(LValue.Fst(LValue.Snd(Ident("p", _), _), _), _), RValue.RExpr(IntLit(4, _), _), _) => true
            case _ => false
        })
    }

    it should "parse all forms of rvalues correctly and reject malformed ones" in {
        checkStmt(parser.parseStmt("int[] qs = [5, 4, 9]"), {
            case Stmt.Declare(Type.ArrayType(Type.IntType), "qs", RValue.ArrayLit(List(IntLit(5, _), IntLit(4, _), IntLit(9, _)), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("int[] ps = []"), {
            case Stmt.Declare(Type.ArrayType(Type.IntType), "ps", RValue.ArrayLit(Nil, _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("int j = pizza[7]"), {
            case Stmt.Declare(Type.IntType, "j", RValue.RExpr(ArrayElem(Ident("pizza", _), List(IntLit(7, _)), _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("pair(int, int) z = newpair(89, 98)"), {
            case Stmt.Declare(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)), "z", RValue.Newpair(IntLit(89, _), IntLit(98, _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("int f = fst k"), {
            case Stmt.Declare(Type.IntType, "f", RValue.Fst(Ident("k", _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("pair(int, pair(int, bool)[]) x = y"), {
            case Stmt.Declare(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.ArrayType(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.BoolType))))), "x", RValue.RExpr(Ident("y", _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("pair(int, int[])[] ferraris = lamborghinis"), {
            case Stmt.Declare(Type.ArrayType(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.ArrayType(Type.IntType)))), "ferraris", RValue.RExpr(Ident("lamborghinis", _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("pair(int[], char[]) heart = brain"), {
            case Stmt.Declare(Type.PairType(PairElemType.Elem(Type.ArrayType(Type.IntType)), PairElemType.Elem(Type.ArrayType(Type.CharType))), "heart", RValue.RExpr(Ident("brain", _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("pair(int, pair) blue = yellow"), {
            case Stmt.Declare(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.ErasedPair), "blue", RValue.RExpr(Ident("yellow", _), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("int c = call f()"), {
            case Stmt.Declare(Type.IntType, "c", RValue.Call("f", Nil, _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("int w = call q(21, 27)"), {
            case Stmt.Declare(Type.IntType, "w", RValue.Call("q", List(IntLit(21, _), IntLit(27, _)), _), _) => true
            case _ => false
        })

        checkStmt(parser.parseStmt("int jumpy = call v(89, false)"), {
            case Stmt.Declare(Type.IntType, "jumpy", RValue.Call("v", List(IntLit(89, _), BoolLit(false, _)), _), _) => true
            case _ => false
        })

        shouldFail(parser.parseStmt("int[] fs = [1,]"))
        shouldFail(parser.parseStmt("pair(int, int) z = (89, 98)"))
        shouldFail(parser.parseStmt("pair(int, pair(int, bool)) cake = spaghetti"))
        shouldFail(parser.parseStmt("int h = call u(5,)"))
    }

    it should "parse programs correctly, with or without functions, and reject malformed ones" in {

        checkProgram(parser.parseFile("begin skip end"), {
            case Program(Nil, Stmt.Skip, _) => true
            case _ => false
        })

        checkProgram(parser.parseFile("begin skip; skip; skip end"), {
            case Program(Nil, Stmt.Seq(List(Stmt.Skip, Stmt.Skip, Stmt.Skip)), _) => true
            case _ => false
        })

        checkProgram(parser.parseFile("begin int f() is return 1 end skip end"), {
            case Program(List(Func(Type.IntType, "f", Nil, Stmt.Return(IntLit(1, _), _), _)), Stmt.Skip, _) => true
            case _ => false
        })

        checkProgram(parser.parseFile("begin int add(int x, int y) is return x + y end skip end"), {
            case Program(List(Func(Type.IntType, "add", List(Param(Type.IntType, "x", _), Param(Type.IntType, "y", _)), Stmt.Return(Add(Ident("x", _), Ident("y", _), _), _), _)), Stmt.Skip, _) => true
            case _ => false
        })

        checkProgram(parser.parseFile("begin int id(int k) is return k end bool trees() is return true end skip end"), {
            case Program(List(Func(Type.IntType, "id", List(Param(Type.IntType, "k", _)), Stmt.Return(Ident("k", _), _), _), Func(Type.BoolType, "trees", Nil, Stmt.Return(BoolLit(true, _), _), _)), Stmt.Skip, _) => true
            case _ => false
        })

        shouldFail(parser.parseFile("skip end"))
        shouldFail(parser.parseFile("begin skip"))
        shouldFail(parser.parseFile("begin end"))
        shouldFail(parser.parseFile("begin int f() is skip end skip end"))
        shouldFail(parser.parseFile("begin int f() return 7 end skip end"))
        shouldFail(parser.parseFile("begin int sin() is return 9 skip end"))
        shouldFail(parser.parseFile("begin int f( is return 1 end skip end"))
        shouldFail(parser.parseFile("begin volvo() is return 6 end skip end"))
        shouldFail(parser.parseFile("begin int () is return 88 end skip end"))
        shouldFail(parser.parseFile("begin int height(int b,) is return b end skip end"))
        shouldFail(parser.parseFile("begin int f(, int x) is return x end skip end"))
        shouldFail(parser.parseFile("begin int f(bool x,, int y) is return x end skip end"))
        shouldFail(parser.parseFile("begin int f(x) is return 99 end skip end"))
        shouldFail(parser.parseFile("begin int f(int) is return 2 end skip end"))
        shouldFail(parser.parseFile("begin skip int f() is return 99 end end"))
    }
}