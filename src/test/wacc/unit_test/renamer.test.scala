package unit_test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.ast.{Error => _, _}
import wacc.Pos
import scala.collection.mutable
import wacc.renamer._
import wacc.renamer

class RenamerTests extends AnyFlatSpec {

    val p = Pos(0, 0)

    def freshState(): (mutable.Map[String, Int], mutable.Builder[Error, List[Error]]) = {
        val globalIds = mutable.Map.empty[String, Int]
        val errs = List.newBuilder[renamer.Error]
        (globalIds, errs)
    }

    it should "rename identifiers in expressions" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        
        val scope = RenamerScope(mutable.Map.empty, Map("x" -> Renamed("x", 0, Type.IntType)), Set.empty)
        
        val input: Expr[String] = Add(Ident("x", p), IntLit(5, p), p)
        val result = rename(input)(using scope, state)
        
        result shouldBe Add(Ident(Renamed("x", 0, Type.IntType), p), IntLit(5, p), p)
        errs.result() shouldBe empty
    }

    it should "detect out-of-scope identifiers in expressions" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        given scope: RenamerScope = RenamerScope(mutable.Map.empty, Map.empty, Set.empty)
        
        val input: Expr[String] = Ident("x", p)
        val _ = rename(input)(using scope, state)
        
        errs.result() should have size 1
        errs.result().head shouldBe Error.OutOfScope("x", p)
    }

    it should "rename nested expressions" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        
        val scope = RenamerScope(mutable.Map.empty, Map(
            "x" -> Renamed("x", 0, Type.IntType),
            "y" -> Renamed("y", 0, Type.IntType)
        ), Set.empty)
        
        val input: Expr[String] = Mul(Add(Ident("x", p), Ident("y", p), p), IntLit(2, p), p)
        val result = rename(input)(using scope, state)
        
        result shouldBe Mul(
            Add(Ident(Renamed("x", 0, Type.IntType), p), Ident(Renamed("y", 0, Type.IntType), p), p),
            IntLit(2, p),
            p
        )
        errs.result() shouldBe empty
    }

    it should "not rename literals" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        given scope: RenamerScope = RenamerScope(mutable.Map.empty, Map.empty, Set.empty)
        
        val input: Expr[String] = Add(IntLit(5, p), BoolLit(true, p), p)
        val result = rename(input)(using scope, state)
        
        result shouldBe Add(IntLit(5, p), BoolLit(true, p), p)
        errs.result() shouldBe empty
    }

    it should "rename a simple variable declaration" in {
        val (ids, errs) = freshState()
        val input = Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(5, p), p), p)
        
        val renamed = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        renamed shouldBe Stmt.Declare(Type.IntType, Renamed("x", 0, Type.IntType), RValue.RExpr(IntLit(5, p), p), p)
        errs.result() shouldBe empty
    }

    it should "rename multiple declarations with different names" in {
        val (ids, errs) = freshState()
        val input = Stmt.Seq(List(
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(5, p), p), p),
            Stmt.Declare(Type.BoolType, "y", RValue.RExpr(BoolLit(true, p), p), p)
        ))
        
        val renamed = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        renamed match {
            case Stmt.Seq(List(
                Stmt.Declare(Type.IntType, Renamed("x", 0, Type.IntType), _, _),
                Stmt.Declare(Type.BoolType, Renamed("y", 0, Type.BoolType), _, _)
            )) => succeed
            case _ => fail(s"Unexpected result: $renamed")
        }
        errs.result() shouldBe empty
    }

    it should "rename shadowing variables correctly" in {
        val (ids, errs) = freshState()
        val input = Stmt.Seq(List(
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(5, p), p), p),
            Stmt.Begin(
                Stmt.Declare(Type.BoolType, "x", RValue.RExpr(BoolLit(true, p), p), p),
                p
            )
        ))
        
        val renamed = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        renamed match {
            case Stmt.Seq(List(
                Stmt.Declare(Type.IntType, Renamed("x", 0, _), _, _),
                Stmt.Begin(Stmt.Declare(Type.BoolType, Renamed("x", 1, _), _, _), _)
            )) => succeed
            case _ => fail(s"Unexpected result: $renamed")
        }
        errs.result() shouldBe empty
    }

    it should "detect out-of-scope variable usage" in {
        val (ids, errs) = freshState()
        val input = Stmt.Read(Ident("x", p), p) 
        
        val _ = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        errs.result() should not be empty
        errs.result().head shouldBe a[renamer.Error.OutOfScope]
    }

    it should "detect redeclaration in same scope" in {
        val (ids, errs) = freshState()
        val input = Stmt.Seq(List(
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(5, p), p), p),
            Stmt.Declare(Type.BoolType, "x", RValue.RExpr(BoolLit(true, p), p), p)
        ))
        
        val _ = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        errs.result() should not be empty
        errs.result().head shouldBe a[renamer.Error.Redeclaration]
    }

    it should "correctly rename variable uses" in {
        val (ids, errs) = freshState()
        val input = Stmt.Seq(List(
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(5, p), p), p),
            Stmt.Assign(Ident("x", p), RValue.RExpr(IntLit(10, p), p), p)
        ))
        
        val renamed = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        renamed match {
            case Stmt.Seq(List(
                Stmt.Declare(Type.IntType, Renamed("x", 0, _), _, _),
                Stmt.Assign(Ident(Renamed("x", 0, _), _), _, _)
            )) => succeed
            case _ => fail(s"Unexpected result: $renamed")
        }
        errs.result() shouldBe empty
    }

    it should "handle nested scopes correctly" in {
        val (ids, errs) = freshState()
        val input = Stmt.Seq(List(
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(1, p), p), p),
            Stmt.Begin(Stmt.Seq(List(
                Stmt.Declare(Type.IntType, "y", RValue.RExpr(Ident("x", p), p), p),  
                Stmt.Assign(Ident("x", p), RValue.RExpr(IntLit(2, p), p), p)     
            )), p)
        ))
        
        val _ = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        errs.result() shouldBe empty
    }

    it should "rename function parameters with unique ids" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        
        val func = Func(Type.IntType, "add", 
            List(Param(Type.IntType, "x", p), Param(Type.IntType, "y", p)),
            Stmt.Return(Add(Ident("x", p), Ident("y", p), p), p), p)
        
        val renamed = rename(func)(using RenamerScope(mutable.Map.empty, Map.empty, Set("add")), state)
        
        renamed.params match {
            case List(Param(_, Renamed("x", 0, _), _), Param(_, Renamed("y", 0, _), _)) => succeed
            case _ => fail(s"Unexpected params: ${renamed.params}")
        }
        errs.result() shouldBe empty
    }

    it should "isolate function body scope from outer scope" in {
        val (ids, errs) = freshState()
        ids("x") = 1
        given state: RenamerState = RenamerState(ids, errs)
        
        val outerScope = RenamerScope(mutable.Map("x" -> Renamed("x", 0, Type.IntType)), Map.empty, Set("f"))
        
        val func = Func(Type.IntType, "f", Nil, 
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(1, p), p), p), p)
        
        val renamed = rename(func)(using outerScope, state)
        
        renamed.body match {
            case Stmt.Declare(_, Renamed("x", 1, _), _, _) => succeed
            case _ => fail(s"Inner x should have id 1, got: ${renamed.body}")
        }
    }

    it should "rename array literal elements" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        
        val scope = RenamerScope(mutable.Map.empty, Map("a" -> Renamed("a", 0, Type.IntType)), Set.empty)
        val input: RValue[String] = RValue.ArrayLit(List(Ident("a", p), IntLit(1, p)), p)
        
        val result = rename(input)(using scope, state)
        
        result match {
            case RValue.ArrayLit(List(Ident(Renamed("a", 0, _), _), IntLit(1, _)), _) => succeed
            case _ => fail(s"Unexpected: $result")
        }
        errs.result() shouldBe empty
    }

    it should "rename newpair expressions" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        
        val scope = RenamerScope(mutable.Map.empty, Map(
            "x" -> Renamed("x", 0, Type.IntType),
            "y" -> Renamed("y", 0, Type.BoolType)
        ), Set.empty)
        
        val input: RValue[String] = RValue.Newpair(Ident("x", p), Ident("y", p), p)
        val result = rename(input)(using scope, state)
        
        result match {
            case RValue.Newpair(Ident(Renamed("x", 0, _), _), Ident(Renamed("y", 0, _), _), _) => succeed
            case _ => fail(s"Unexpected: $result")
        }
    }

    it should "rename function call arguments" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        
        val scope = RenamerScope(mutable.Map.empty, Map("n" -> Renamed("n", 0, Type.IntType)), Set("f"))
        val input: RValue[String] = RValue.Call("f", List(Ident("n", p)), p)
        
        val result = rename(input)(using scope, state)
        
        result match {
            case RValue.Call("f", List(Ident(Renamed("n", 0, _), _)), _) => succeed
            case _ => fail(s"Unexpected: $result")
        }
    }

    it should "rename nested fst and snd lvalues" in {
        val (ids, errs) = freshState()
        given state: RenamerState = RenamerState(ids, errs)
        
        val pairType = Type.PairType(PairElemType.Elem(Type.IntType), PairElemType.Elem(Type.IntType))
        val scope = RenamerScope(mutable.Map.empty, Map("p" -> Renamed("p", 0, pairType)), Set.empty)
        
        val input: LValue[String] = LValue.Fst(LValue.Snd(Ident("p", p), p), p)
        val result = rename(input)(using scope, state)
        
        result match {
            case LValue.Fst(LValue.Snd(Ident(Renamed("p", 0, _), _), _), _) => succeed
            case _ => fail(s"Unexpected: $result")
        }
    }

    it should "give if branches isolated scopes" in {
        val (ids, errs) = freshState()
        val input = Stmt.Seq(List(
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(1, p), p), p),
            Stmt.If(BoolLit(true, p),
                Stmt.Declare(Type.IntType, "y", RValue.RExpr(IntLit(2, p), p), p),
                Stmt.Declare(Type.IntType, "y", RValue.RExpr(IntLit(3, p), p), p),
                p)
        ))
        
        val renamed = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        renamed match {
            case Stmt.Seq(List(_, Stmt.If(_,
                Stmt.Declare(_, Renamed("y", 0, _), _, _),
                Stmt.Declare(_, Renamed("y", 1, _), _, _), _))) => succeed
            case _ => fail(s"Both branches should have separate y declarations: $renamed")
        }
        errs.result() shouldBe empty
    }

    it should "give while body an isolated scope" in {
        val (ids, errs) = freshState()
        val input = Stmt.Seq(List(
            Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(1, p), p), p),
            Stmt.While(BoolLit(true, p),
                Stmt.Declare(Type.IntType, "x", RValue.RExpr(IntLit(2, p), p), p), p)
        ))
        
        val renamed = rename(input)(using RenamerScope(mutable.Map.empty, Map.empty, Set.empty), RenamerState(ids, errs))
        
        renamed match {
            case Stmt.Seq(List(
                Stmt.Declare(_, Renamed("x", 0, _), _, _),
                Stmt.While(_, Stmt.Declare(_, Renamed("x", 1, _), _, _), _))) => succeed
            case _ => fail(s"While body x should shadow outer x: $renamed")
        }
        errs.result() shouldBe empty
    }
}