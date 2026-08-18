package unit_test

import backend.stackMachine
import backend.Printer
import wacc.ast.Type
import wacc.ast.PairElemType
import wacc.renamer.Renamed
import wacc.TypedStmt
import wacc.TypedLValue
import wacc.TypedFunc
import wacc.TypedProgram
import wacc.TypedExpr.*
import wacc.TypedStmt.*
import wacc.TypedRValue.*

object BackendTestSuite {
    private var count = 0

    private def dummyName(name: String, ty: Type): Renamed = {
        val r = Renamed(name, count, ty)
        count += 1
        r
    }

    private def block(stmts: TypedStmt*): TypedStmt = Seq(stmts.toList)

    private def runTest(name: String, body: TypedStmt, funcs:List[TypedFunc] = Nil): Unit = {
        count = 0
        println(s"\n\nTEST: $name")
        val program = TypedProgram(funcs, body)
        val code = stackMachine.compileProgram(program)
        Printer.print(code)
    }

    @main def runBackendTests(): Unit = {
        testArithmetic()
        testIf()
        testWhile()
        testNested()
        testShortCircuit()
        testArrayLit()
        testArrayAssign()
        testArrayLen()
        testNewpair()
        testPairAssign()
        testNestedPair()
        testFunction()
        testProgram()
    }

    private def testArithmetic(): Unit = {
        runTest(
            "Arithmetic",
            Print(Add(IntLit(5), Mul(IntLit(2), IntLit(3))), false)
        )
    }

    private def testIf(): Unit = {
        val x = dummyName("x", Type.IntType)

        runTest(
            "Simple if",
            block(
                Declare(Type.IntType, x, RExpr(IntLit(5))),
                If(
                    Greater(Ident(x), IntLit(3)),
                    Print(IntLit(1), false),
                    Print(IntLit(0), false)
                )
            )
        )
    }

    private def testWhile(): Unit = {
        val x = dummyName("x", Type.IntType)

        runTest(
            "Simple while",
            block(
                Declare(Type.IntType, x, RExpr(IntLit(0))),
                While(
                    Less(Ident(x), IntLit(3)),
                    Assign(
                        TypedLValue.Ident(x),
                        RExpr(Add(Ident(x), IntLit(1)))
                    )
                )
            )
        )
    }

    private def testNested(): Unit = {
        val x = dummyName("x", Type.IntType)

        runTest(
            "Nested while and if",
            block(
                Declare(Type.IntType, x, RExpr(IntLit(0))),
                While(
                    Less(Ident(x), IntLit(5)),
                    block(
                        If(
                            Equals(Mod(Ident(x), IntLit(2)), IntLit(0)),
                            Print(Ident(x), false),
                            Skip
                        ),
                        Assign(
                            TypedLValue.Ident(x),
                            RExpr(Add(Ident(x), IntLit(1)))
                        )
                    )
                )
            )
        )
    }

    private def testShortCircuit(): Unit = {
        val a = dummyName("a", Type.BoolType)
        val b = dummyName("b", Type.BoolType)

        runTest(
            "Short circuiting and, or",
            block(
                Declare(Type.BoolType, a, RExpr(BoolLit(false))),
                Declare(Type.BoolType, b, RExpr(BoolLit(true))),
                Print(And(Ident(a), Ident(b)), false),
                Print(Or(Ident(a), Ident(b)), false)
            )
        )
    }

    private def testArrayLit(): Unit = {
        val arr = dummyName("arr", Type.ArrayType(Type.IntType))

        runTest(
            "Array literal and accessing",
            block(
                Declare(
                    Type.ArrayType(Type.IntType),
                    arr,
                    ArrayLit(List(IntLit(1), IntLit(2), IntLit(3)), Type.ArrayType(Type.IntType))
                ),
                Print(ArrayElem(arr, List(IntLit(0)), Type.IntType), false),
                Print(ArrayElem(arr, List(IntLit(1)), Type.IntType), false),
                Print(ArrayElem(arr, List(IntLit(2)), Type.IntType), false)
            )
        )
    }

    private def testArrayAssign(): Unit = {
        val arr = dummyName("arr", Type.ArrayType(Type.IntType))

        runTest(
            "Array assign",
            block(
                Declare(
                    Type.ArrayType(Type.IntType),
                    arr,
                    ArrayLit(List(IntLit(0), IntLit(0), IntLit(0)), Type.ArrayType(Type.IntType))
                ),
                Assign(
                    TypedLValue.ArrayElem(arr, List(IntLit(1)), Type.IntType),
                    RExpr(IntLit(99))
                ),
                Print(ArrayElem(arr, List(IntLit(0)), Type.IntType), false),
                Print(ArrayElem(arr, List(IntLit(1)), Type.IntType), false),
                Print(ArrayElem(arr, List(IntLit(2)), Type.IntType), false)
            )
        )
    }

    private def testArrayLen(): Unit = {
        val arr = dummyName("arr", Type.ArrayType(Type.IntType))

        runTest(
            "Array length",
            block(
                Declare(
                    Type.ArrayType(Type.IntType),
                    arr,
                    ArrayLit(List(IntLit(10), IntLit(20), IntLit(30), IntLit(40)), Type.ArrayType(Type.IntType))
                ),
                Print(Len(Ident(arr)), false)
            )
        )
    }

    private def testNewpair(): Unit = {
        val p = dummyName("p", Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)))
        val f = dummyName("f", Type.IntType)
        val s = dummyName("s", Type.IntType)

        runTest(
            "Newpair and fst and snd",
            block(
                Declare(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)), p,
                    Newpair(IntLit(42), IntLit(99), p.ty)
                ),
                Declare(Type.IntType, f, Fst(TypedLValue.Ident(p), Type.IntType)),
                Declare(Type.IntType, s, Snd(TypedLValue.Ident(p), Type.IntType)),
                Print(Ident(f), false),
                Print(Ident(s), false)
            )
        )
    }

    private def testPairAssign(): Unit = {
        val p = dummyName("p", Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)))
        val f = dummyName("f", Type.IntType)
        val s = dummyName("s", Type.IntType)

        runTest(
            "Pair field assignment",
            block(
                Declare(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)), p,
                    Newpair(IntLit(1), IntLit(2), p.ty)
                ),
                Assign(TypedLValue.Fst(TypedLValue.Ident(p), Type.IntType), RExpr(IntLit(100))),
                Assign(TypedLValue.Snd(TypedLValue.Ident(p), Type.IntType), RExpr(IntLit(200))),
                Declare(Type.IntType, f, Fst(TypedLValue.Ident(p), Type.IntType)),
                Declare(Type.IntType, s, Snd(TypedLValue.Ident(p), Type.IntType)),
                Print(Ident(f), false),
                Print(Ident(s), false)
            )
        )
    }

    private def testNestedPair(): Unit = {
        val inner = dummyName("inner", Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)))
        val outer = dummyName(
            "outer",
            Type.PairType(
                PairElemType.Elem(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType))),
                PairElemType.Elem(Type.IntType)
            )
        )
        val s = dummyName("s", Type.IntType)

        runTest(
            "Nested pair",
            block(
                Declare(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)), inner,
                    Newpair(IntLit(10), IntLit(20), inner.ty)
                ),
                Declare(outer.ty.asInstanceOf[Type], outer, Newpair(Ident(inner), IntLit(99), outer.ty)),
                Declare(Type.IntType, s, Snd(TypedLValue.Ident(outer), Type.IntType)),
                Print(Ident(s), false)
            )
        )
    }

    private def testFunction(): Unit = {
        val x = dummyName("x", Type.IntType)
        val param = dummyName("n", Type.IntType)
        val func = TypedFunc("double", List(param), Return(Add(Ident(param), Ident(param))))
        val body =
            block(
                Declare(Type.IntType, x, Call("double", List(IntLit(21)), Type.IntType)),
                Print(Ident(x), false)
            )

        runTest("Function call", body, List(func))
    }

    private def testProgram(): Unit = {
        val x = dummyName("x", Type.IntType)
        val acc = dummyName("acc", Type.IntType)
        val n = dummyName("n", Type.IntType)
        val func =
            TypedFunc(
                "triangle",
                List(n),
                Return(Div(Mul(Ident(n), Add(Ident(n), IntLit(1))), IntLit(2)))
            )
        val body =
            block(
                Declare(Type.IntType, x, Call("triangle", List(IntLit(5)), Type.IntType)),
                Print(Ident(x), false),
                Declare(Type.IntType, acc, Call("triangle", List(IntLit(10)), Type.IntType)),
                Print(Ident(acc), false)
            )

        runTest("Full program", body, List(func))
    }
}