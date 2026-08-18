package unit_test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.ast.{Error => _, _}
import wacc.Pos
import scala.collection.mutable
import wacc.renamer.Renamed
import wacc._

class TypeCheckerTests extends AnyFlatSpec {

    val p = Pos(0, 0)

    def freshState(): TCState = {
        val errs = List.newBuilder[Error]
        val funcTable = mutable.Map.empty[String, FuncSig]
        TCState(errs, funcTable)
    }

    it should "assign correct types to literals" in {
        given st: TCState = freshState()
        
        val (intTyped, intTy) = typeChecker.checkExpr(IntLit(42, p), Constraint.Infer)
        intTyped.ty shouldBe Type.IntType
        intTy shouldBe Some(Type.IntType)
        
        val (boolTyped, boolTy) = typeChecker.checkExpr(BoolLit(true, p), Constraint.Infer)
        boolTyped.ty shouldBe Type.BoolType
        boolTy shouldBe Some(Type.BoolType)
        
        val (charTyped, charTy) = typeChecker.checkExpr(CharLit('a', p), Constraint.Infer)
        charTyped.ty shouldBe Type.CharType
        charTy shouldBe Some(Type.CharType)
        
        val (strTyped, strTy) = typeChecker.checkExpr(StrLit("hello", p), Constraint.Infer)
        strTyped.ty shouldBe Type.StringType
        strTy shouldBe Some(Type.StringType)
        
        val (pairTyped, pairTy) = typeChecker.checkExpr(PairLit(null, p), Constraint.Infer)
        pairTyped.ty shouldBe Type.Pair
        pairTy shouldBe Some(Type.Pair)
        
        st.errors shouldBe empty
    }

    it should "use variable type from renamer for identifiers" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        val (typed, ty) = typeChecker.checkExpr(Ident(x, p), Constraint.Infer)
        typed.ty shouldBe Type.IntType
        ty shouldBe Some(Type.IntType)
        st.errors shouldBe empty
    }

    it should "reduce array dimension when indexing" in {
        given st: TCState = freshState()
        val arr = Renamed("arr", 0, Type.ArrayType(Type.IntType))
        val (typed, ty) = typeChecker.checkExpr(ArrayElem(Ident(arr, p), List(IntLit(0, p)), p), Constraint.Infer)
        typed.ty shouldBe Type.IntType
        ty shouldBe Some(Type.IntType)
        st.errors shouldBe empty
    }

    it should "reduce nested array dimensions correctly" in {
        given st: TCState = freshState()
        val arr2d = Renamed("arr2d", 0, Type.ArrayType(Type.ArrayType(Type.CharType)))
        val (typed, ty) = typeChecker.checkExpr(ArrayElem(Ident(arr2d, p), List(IntLit(0, p), IntLit(1, p)), p), Constraint.Infer)
        typed.ty shouldBe Type.CharType
        ty shouldBe Some(Type.CharType)
        st.errors shouldBe empty
    }

    it should "check unary operator operand types" in {
        given st: TCState = freshState()
        
        val (notTyped, _) = typeChecker.checkExpr(Not(BoolLit(true, p), p), Constraint.Infer)
        notTyped.ty shouldBe Type.BoolType
        
        val (negTyped, _) = typeChecker.checkExpr(Neg(IntLit(5, p), p), Constraint.Infer)
        negTyped.ty shouldBe Type.IntType
        
        val arr = Renamed("arr", 0, Type.ArrayType(Type.IntType))
        val (lenTyped, _) = typeChecker.checkExpr(Len(Ident(arr, p), p), Constraint.Infer)
        lenTyped.ty shouldBe Type.IntType
        
        val (ordTyped, _) = typeChecker.checkExpr(Ord(CharLit('a', p), p), Constraint.Infer)
        ordTyped.ty shouldBe Type.IntType
        
        val (chrTyped, _) = typeChecker.checkExpr(Chr(IntLit(65, p), p), Constraint.Infer)
        chrTyped.ty shouldBe Type.CharType
        
        st.errors shouldBe empty
    }

    it should "require int operands for arithmetic operators" in {
        given st: TCState = freshState()
        
        typeChecker.checkExpr(Add(IntLit(1, p), IntLit(2, p), p), Constraint.Infer)._1.ty shouldBe Type.IntType
        typeChecker.checkExpr(Sub(IntLit(5, p), IntLit(3, p), p), Constraint.Infer)._1.ty shouldBe Type.IntType
        typeChecker.checkExpr(Mul(IntLit(4, p), IntLit(2, p), p), Constraint.Infer)._1.ty shouldBe Type.IntType
        typeChecker.checkExpr(Div(IntLit(8, p), IntLit(2, p), p), Constraint.Infer)._1.ty shouldBe Type.IntType
        typeChecker.checkExpr(Mod(IntLit(7, p), IntLit(3, p), p), Constraint.Infer)._1.ty shouldBe Type.IntType
        
        st.errors shouldBe empty
    }

    it should "require ordered types for comparison operators" in {
        given st: TCState = freshState()
        
        typeChecker.checkExpr(Less(IntLit(1, p), IntLit(2, p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        typeChecker.checkExpr(LessEq(CharLit('a', p), CharLit('b', p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        typeChecker.checkExpr(Greater(IntLit(5, p), IntLit(3, p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        typeChecker.checkExpr(GreaterEq(IntLit(5, p), IntLit(5, p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        
        st.errors shouldBe empty
    }

    it should "require matching types for equality operators" in {
        given st: TCState = freshState()
        
        typeChecker.checkExpr(Equals(IntLit(1, p), IntLit(1, p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        typeChecker.checkExpr(Equals(BoolLit(true, p), BoolLit(false, p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        typeChecker.checkExpr(NotEquals(CharLit('x', p), CharLit('y', p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        
        st.errors shouldBe empty
    }

    it should "require bool operands for logical operators" in {
        given st: TCState = freshState()
        
        typeChecker.checkExpr(And(BoolLit(true, p), BoolLit(false, p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        typeChecker.checkExpr(Or(BoolLit(true, p), BoolLit(false, p), p), Constraint.Infer)._1.ty shouldBe Type.BoolType
        
        st.errors shouldBe empty
    }

    it should "report error on arithmetic with wrong types" in {
        given st: TCState = freshState()
        typeChecker.checkExpr(Add(BoolLit(true, p), IntLit(1, p), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "report error on logical ops with wrong types" in {
        given st: TCState = freshState()
        typeChecker.checkExpr(And(IntLit(1, p), IntLit(2, p), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "report error on comparison with non-ordered types" in {
        given st: TCState = freshState()
        typeChecker.checkExpr(Less(BoolLit(true, p), BoolLit(false, p), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "report error on not with non-bool" in {
        given st: TCState = freshState()
        typeChecker.checkExpr(Not(IntLit(1, p), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "report error on neg with non-int" in {
        given st: TCState = freshState()
        typeChecker.checkExpr(Neg(BoolLit(true, p), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "type check nested expressions correctly" in {
        given st: TCState = freshState()
        val nested = Add(Mul(IntLit(2, p), IntLit(3, p), p), Sub(IntLit(10, p), IntLit(4, p), p), p)
        val (typed, ty) = typeChecker.checkExpr(nested, Constraint.Infer)
        typed.ty shouldBe Type.IntType
        ty shouldBe Some(Type.IntType)
        st.errors shouldBe empty
    }

    it should "type check complex boolean expressions" in {
        given st: TCState = freshState()
        val complex = And(Less(IntLit(1, p), IntLit(2, p), p), Or(BoolLit(true, p), Equals(CharLit('a', p), CharLit('b', p), p), p), p)
        val (typed, ty) = typeChecker.checkExpr(complex, Constraint.Infer)
        typed.ty shouldBe Type.BoolType
        ty shouldBe Some(Type.BoolType)
        st.errors shouldBe empty
    }

    it should "pass declaration when types match" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        typeChecker.checkStmt(Stmt.Declare(Type.IntType, x, RValue.RExpr(IntLit(5, p), p), p))
        st.errors shouldBe empty
    }

    it should "fail declaration when types mismatch" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        typeChecker.checkStmt(Stmt.Declare(Type.IntType, x, RValue.RExpr(BoolLit(true, p), p), p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "check assignment type compatibility" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        typeChecker.checkStmt(Stmt.Assign(Ident(x, p), RValue.RExpr(IntLit(10, p), p), p))
        st.errors shouldBe empty
    }

    it should "fail assignment when types mismatch" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        typeChecker.checkStmt(Stmt.Assign(Ident(x, p), RValue.RExpr(BoolLit(true, p), p), p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "require int or char for read" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        val c = Renamed("c", 0, Type.CharType)
        typeChecker.checkStmt(Stmt.Read(Ident(x, p), p))
        typeChecker.checkStmt(Stmt.Read(Ident(c, p), p))
        st.errors shouldBe empty
    }

    it should "fail read on non-ordered type" in {
        given st: TCState = freshState()
        val b = Renamed("b", 0, Type.BoolType)
        typeChecker.checkStmt(Stmt.Read(Ident(b, p), p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "require heap type for free" in {
        given st: TCState = freshState()
        val arr = Renamed("arr", 0, Type.ArrayType(Type.IntType))
        val pair = Renamed("pair", 0, Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType)))
        typeChecker.checkStmt(Stmt.Free(Ident(arr, p), p))
        typeChecker.checkStmt(Stmt.Free(Ident(pair, p), p))
        st.errors shouldBe empty
    }

    it should "fail free on non-heap type" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        typeChecker.checkStmt(Stmt.Free(Ident(x, p), p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.CannotFreeNonHeap]
    }

    it should "require int for exit" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.Exit(IntLit(0, p), p))
        st.errors shouldBe empty
    }

    it should "fail exit with non-int" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.Exit(BoolLit(true, p), p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "fail return outside function" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.Return(IntLit(0, p), p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.ReturnOutsideFunction]
    }

    it should "check return type matches function return type" in {
        given st: TCState = freshState()
        st.enterFunc(Type.IntType)
        typeChecker.checkStmt(Stmt.Return(IntLit(42, p), p))
        st.errors shouldBe empty
    }

    it should "fail return when type mismatches function return type" in {
        given st: TCState = freshState()
        st.enterFunc(Type.IntType)
        typeChecker.checkStmt(Stmt.Return(BoolLit(true, p), p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "require bool condition for if" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.If(BoolLit(true, p), Stmt.Skip, Stmt.Skip, p))
        st.errors shouldBe empty
    }

    it should "fail if with non-bool condition" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.If(IntLit(1, p), Stmt.Skip, Stmt.Skip, p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "require bool condition for while" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.While(BoolLit(true, p), Stmt.Skip, p))
        st.errors shouldBe empty
    }

    it should "fail while with non-bool condition" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.While(IntLit(1, p), Stmt.Skip, p))
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "accept any type for print" in {
        given st: TCState = freshState()
        typeChecker.checkStmt(Stmt.Print(IntLit(42, p), false, p))
        typeChecker.checkStmt(Stmt.Print(BoolLit(true, p), true, p))
        typeChecker.checkStmt(Stmt.Print(CharLit('x', p), false, p))
        typeChecker.checkStmt(Stmt.Print(StrLit("hello", p), true, p))
        st.errors shouldBe empty
    }

    it should "check function call argument types" in {
        given st: TCState = freshState()
        st.funcTable("add") = FuncSig(Type.IntType, List(Type.IntType, Type.IntType))
        val (_, ty) = typeChecker.checkRValue(RValue.Call("add", List(IntLit(1, p), IntLit(2, p)), p), Constraint.Infer)
        ty shouldBe Some(Type.IntType)
        st.errors shouldBe empty
    }

    it should "fail function call with wrong argument types" in {
        given st: TCState = freshState()
        st.funcTable("f") = FuncSig(Type.IntType, List(Type.IntType))
        typeChecker.checkRValue(RValue.Call("f", List(BoolLit(true, p)), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "fail function call with wrong argument count" in {
        given st: TCState = freshState()
        st.funcTable("f") = FuncSig(Type.IntType, List(Type.IntType, Type.IntType))
        typeChecker.checkRValue(RValue.Call("f", List(IntLit(1, p)), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.WrongArgCount]
    }

    it should "fail call to undefined function" in {
        given st: TCState = freshState()
        typeChecker.checkRValue(RValue.Call("unknown", List(), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.UndefinedFunction]
    }

    it should "extract fst element from pair" in {
        given st: TCState = freshState()
        val pair = Renamed("p", 0, Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.BoolType)))
        val (_, ty) = typeChecker.checkRValue(RValue.Fst(Ident(pair, p), p), Constraint.Infer)
        ty shouldBe Some(Type.IntType)
        st.errors shouldBe empty
    }

    it should "extract snd element from pair" in {
        given st: TCState = freshState()
        val pair = Renamed("p", 0, Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.BoolType)))
        val (_, ty) = typeChecker.checkRValue(RValue.Snd(Ident(pair, p), p), Constraint.Infer)
        ty shouldBe Some(Type.BoolType)
        st.errors shouldBe empty
    }

    it should "fail fst on non-pair" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        typeChecker.checkRValue(RValue.Fst(Ident(x, p), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "fail snd on non-pair" in {
        given st: TCState = freshState()
        val x = Renamed("x", 0, Type.IntType)
        typeChecker.checkRValue(RValue.Snd(Ident(x, p), p), Constraint.Infer)
        st.errors should not be empty
        st.errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "infer array literal type" in {
        given st: TCState = freshState()
        val (_, ty) = typeChecker.checkRValue(RValue.ArrayLit(List(IntLit(1, p), IntLit(2, p), IntLit(3, p)), p), Constraint.Infer)
        ty shouldBe Some(Type.ArrayType(Type.IntType))
        st.errors shouldBe empty
    }

    it should "create correct pair type from newpair" in {
        given st: TCState = freshState()
        val (_, ty) = typeChecker.checkRValue(RValue.Newpair(IntLit(1, p), BoolLit(true, p), p), Constraint.Infer)
        ty shouldBe Some(Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.BoolType)))
        st.errors shouldBe empty
    }

    it should "unify pair with pair(T, T)" in {
        val pairTy = Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType))
        val result = Type.Pair === pairTy
        result shouldBe Some(pairTy)
    }

    it should "unify char array with string" in {
        val result = Type.ArrayType(Type.CharType) ~ Type.StringType
        result shouldBe Some(Type.StringType)
    }

    it should "fail on duplicate function definitions" in {
        val prog: Program[Renamed] = Program(
            List(
                Func(Type.IntType, "f", Nil, Stmt.Return(IntLit(1, p), p), p),
                Func(Type.IntType, "f", Nil, Stmt.Return(IntLit(2, p), p), p)
            ),
            Stmt.Skip,
            p
        )
        val (_, errors) = typeChecker.check(prog)
        errors should not be empty
        errors.head shouldBe a[Error.RedefinedFunction]
    }

    it should "type check a valid program end-to-end" in {
        val x = Renamed("x", 0, Type.IntType)
        val prog: Program[Renamed] = Program(Nil, Stmt.Declare(Type.IntType, x, RValue.RExpr(IntLit(42, p), p), p), p)
        val (_, errors) = typeChecker.check(prog)
        errors shouldBe empty
    }

    it should "detect type error in program body" in {
        val x = Renamed("x", 0, Type.IntType)
        val prog: Program[Renamed] = Program(Nil, Stmt.Declare(Type.IntType, x, RValue.RExpr(BoolLit(true, p), p), p), p)
        val (_, errors) = typeChecker.check(prog)
        errors should not be empty
        errors.head shouldBe a[Error.TypeMismatch]
    }

    it should "detect undefined function in program" in {
        val prog: Program[Renamed] = Program(
            Nil,
            Stmt.Declare(Type.IntType, Renamed("x", 0, Type.IntType), RValue.Call("unknown", List(), p), p),
            p
        )
        val (_, errors) = typeChecker.check(prog)
        errors should not be empty
        errors.head shouldBe a[Error.UndefinedFunction]
    }
}
