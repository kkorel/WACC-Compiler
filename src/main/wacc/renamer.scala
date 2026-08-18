package wacc

import ast._
import scala.collection.mutable

object renamer {
    type Unrenamed = String
    case class Renamed(name: String, id: Int, ty: Type)
    
    sealed trait Error {
        def pos: Pos 
        def category: ErrorCategory
        def message: String

        def toSemanticError(fileName: String): SemanticError = SemanticError(fileName, pos, category, message)
    }
    object Error {
        case class OutOfScope(name: String, pos: Pos) extends Error {
            val category = ErrorCategory.Scope
            val message = s"Variable $name Not Defined"
        }

        case class Redeclaration(name: String, pos: Pos) extends Error {
            val category = ErrorCategory.Scope
            val message = s"Variable $name Has Already Been Declared"
        }
    }

    private val OutOfScopeDummy = Renamed("out-of-scope", -1, Type.IntType)

    class RenamerScope(
        val curScope: mutable.Map[Unrenamed, Renamed],
        parentScope: Map[Unrenamed, Renamed],
        val funcNames: Set[Unrenamed]
    ) {
        def lookup(name: Unrenamed): Option[Renamed] = curScope.get(name).orElse(parentScope.get(name))
            
        def newScope: RenamerScope = RenamerScope(mutable.Map.empty, parentScope ++ curScope, funcNames)
    }

    class RenamerState(
        globalIds: mutable.Map[Unrenamed, Int],
        errs: mutable.Builder[Error, List[Error]]
    ) {
        def fresh(name: Unrenamed, ty: Type): Renamed = {
            val id = globalIds.getOrElse(name, 0)
            globalIds(name) = id + 1
            Renamed(name, id, ty)
        }
        
        def emit(err: Error): Unit = errs += err
    }

    private def getFromScope(name: Unrenamed, pos: Pos)(using ctx: RenamerScope, state: RenamerState): Renamed = {
        ctx.lookup(name).getOrElse {
            state.emit(Error.OutOfScope(name, pos))
            OutOfScopeDummy
        }
    }

    def declare(name: Unrenamed, ty: Type, pos: Pos)(using ctx: RenamerScope, state: RenamerState) = {
        if (ctx.curScope.contains(name)) {
            state.emit(Error.Redeclaration(name, pos))
        }
        val qualName = state.fresh(name, ty)
        ctx.curScope(name) = qualName
        qualName
    }

    def rename(prog: Program[String]): (Program[Renamed], List[Error]) = {
        val errs = List.newBuilder[Error]
        val funcNames = prog.fs.map(_.name).toSet

        given state: RenamerState = RenamerState(mutable.Map.empty, errs)
        given scope: RenamerScope = RenamerScope(mutable.Map.empty, Map.empty, funcNames)

        val renamedFuncs = prog.fs.map(rename)
        val renamedBody = rename(prog.body)

        (Program[Renamed](renamedFuncs, renamedBody, prog.pos), errs.result())
    }

    def rename(func: Func[String])(using ctx: RenamerScope, state: RenamerState): Func[Renamed] = {
        val paramScope = ctx.newScope
        val renamedParams = func.params.map(rename(_)(using paramScope, state))
        val bodyScope = paramScope.newScope
        val renamedBody = rename(func.body)(using bodyScope, state)
        
        Func(func.returnType, func.name, renamedParams, renamedBody, func.pos)
    }

    def rename(param: Param[String])(using ctx: RenamerScope, state: RenamerState): Param[Renamed] = {
        val renamed = declare(param.name, param.t, param.pos)
        Param(param.t, renamed, param.pos)
    }

    def rename(stmt: Stmt[String])(using ctx: RenamerScope, state: RenamerState): Stmt[Renamed] = stmt match {
        case Stmt.Skip => Stmt.Skip
        case Stmt.Seq(stmts) => Stmt.Seq(stmts.map(rename))
        case Stmt.Declare(ty, name, x, pos) => 
            val rValRenamed = rename(x)
            val nameRenamed = declare(name, ty, pos)
            Stmt.Declare(ty, nameRenamed, rValRenamed, pos)
        case Stmt.Assign(x, y, pos) => Stmt.Assign(rename(x), rename(y), pos)
        case Stmt.Read(x, pos) => Stmt.Read(rename(x), pos)
        case Stmt.Free(x, pos) => Stmt.Free(rename(x), pos)
        case Stmt.Return(x, pos) => Stmt.Return(rename(x), pos)
        case Stmt.Exit(x, pos) => Stmt.Exit(rename(x), pos)
        case Stmt.Print(x, n, pos) => Stmt.Print(rename(x), n, pos)
        case Stmt.If(cond, thenx, elsey, pos) => Stmt.If(
            rename(cond), 
            rename(thenx)(using ctx.newScope),
            rename(elsey)(using ctx.newScope),
            pos
        )
        case Stmt.While(cond, x, pos) => Stmt.While(
            rename(cond), 
            rename(x)(using ctx.newScope),
            pos
        )
        case Stmt.Begin(x, pos) => Stmt.Begin(rename(x)(using ctx.newScope), pos)
    }

    def rename(expr: Expr[String])(using RenamerScope, RenamerState): Expr[Renamed] = expr match {
        case IntLit(x, pos) => IntLit(x, pos)
        case BoolLit(x, pos) => BoolLit(x, pos)
        case CharLit(x, pos) => CharLit(x, pos)
        case StrLit(x, pos) => StrLit(x, pos)
        case PairLit(x, pos) => PairLit(x, pos)
        case Ident(name, pos) => Ident(getFromScope(name, pos), pos)
        case ArrayElem(Ident(name, identPos), exprs, pos) => 
            ArrayElem(Ident(getFromScope(name, identPos), identPos), exprs.map(rename), pos)
        
        case Not(x, pos) => Not(rename(x), pos)
        case Neg(x, pos) => Neg(rename(x), pos)
        case Len(x, pos) => Len(rename(x), pos)
        case Ord(x, pos) => Ord(rename(x), pos)
        case Chr(x, pos) => Chr(rename(x), pos)

        case Mul(x, y, pos) => Mul(rename(x), rename(y), pos)
        case Div(x, y, pos) => Div(rename(x), rename(y), pos)
        case Mod(x, y, pos) => Mod(rename(x), rename(y), pos)
        case Add(x, y, pos) => Add(rename(x), rename(y), pos)
        case Sub(x, y, pos) => Sub(rename(x), rename(y), pos)
        case Greater(x, y, pos) => Greater(rename(x), rename(y), pos)
        case GreaterEq(x, y, pos) => GreaterEq(rename(x), rename(y), pos)
        case Less(x, y, pos) => Less(rename(x), rename(y), pos)
        case LessEq(x, y, pos) => LessEq(rename(x), rename(y), pos)
        case Equals(x, y, pos) => Equals(rename(x), rename(y), pos)
        case NotEquals(x, y, pos) => NotEquals(rename(x), rename(y), pos)
        case And(x, y, pos) => And(rename(x), rename(y), pos)
        case Or(x, y, pos) => Or(rename(x), rename(y), pos)
    }

    def rename(lval: LValue[String])(using RenamerScope, RenamerState): LValue[Renamed] = lval match {
        case Ident(name, pos) => Ident(getFromScope(name, pos), pos)
        case ArrayElem(Ident(name, identPos), exprs, pos) => 
            ArrayElem(Ident(getFromScope(name, identPos), identPos), exprs.map(rename), pos)
        case LValue.Fst(x, pos) => LValue.Fst(rename(x), pos)
        case LValue.Snd(y, pos) => LValue.Snd(rename(y), pos)
    }

    def rename(rval: RValue[String])(using RenamerScope, RenamerState): RValue[Renamed] = rval match {
        case RValue.RExpr(expr, pos) => RValue.RExpr(rename(expr), pos)
        case RValue.ArrayLit(elems, pos) => RValue.ArrayLit(elems.map(rename), pos)
        case RValue.Newpair(x, y, pos) => RValue.Newpair(rename(x), rename(y), pos)
        case RValue.Fst(lval, pos) => RValue.Fst(rename(lval), pos)
        case RValue.Snd(lval, pos) => RValue.Snd(rename(lval), pos)
        case RValue.Call(f, args, pos) => RValue.Call(f, args.map(rename), pos)
    }
}