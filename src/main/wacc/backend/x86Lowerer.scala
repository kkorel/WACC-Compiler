package backend
import scala.collection.mutable.ListBuffer

/* 
x86Lowerer:
    - converts general ir to specific x86 ir
    - handles runtime errors
    - builds routines required
    - converts complicated instrs like 'add' to simple x86
    - Stage 2
*/
object x86Lowerer {

    // type defs for longer types
    private type Needed = scala.collection.mutable.Set[SupportRoutine]
    private type StringLiterals = scala.collection.mutable.LinkedHashMap[String, String]

    // frame state class used for offsets of variables
    private class FrameState(val varOffsets: Map[wacc.renamer.Renamed, Int])

    // Converts whole program to x86 IR, sets up header and calls lowerBody
    def lowerProgram(ir: List[Instr]): List[x86Instr] = {
        given labeller: Labeller = Labeller()
        val out = ListBuffer[x86Instr]()
        out += IntelSyntax
        out += GloblMain
        out ++= lowerBody(ir)
        out.toList
    }

    // Calculates offset for variables before traversing body
    private def computeVarOffsets(instrs: List[Instr]): Map[wacc.renamer.Renamed, Int] =
        val offsets = scala.collection.mutable.Map[wacc.renamer.Renamed, Int]()
        var nextOffset = zeroOffset
        instrs.foreach {
            case EnterFrameI(params, _) =>
                nextOffset = zeroOffset
            case StoreVar(name) if !offsets.contains(name) =>
                nextOffset += incrementOffset
                offsets(name) = nextOffset * WordSize
            case _ => ()
        }
        offsets.toMap

    // Converts main body to ir using list buffer, prepends rodata and append required routines
    def lowerBody(ir: List[Instr])(using labeller: Labeller): List[x86Instr] = {
        val stringLiterals = scala.collection.mutable.LinkedHashMap[String, String]()
        val varOffsets = computeVarOffsets(ir)     
        val frame = FrameState(varOffsets)     
        val needed = scala.collection.mutable.Set[SupportRoutine]()
        val out = ListBuffer[x86Instr]()

        ir.foreach(lowerInstr(_, needed, stringLiterals, frame, out))

        val rodata = if stringLiterals.isEmpty then Nil else
            List(SectionRodata) ++
            stringLiterals.flatMap { (value, label) =>
                List(RodataInt(value.length), RodataString(NamedLabel(label), value))
            }.toList ++
            List(SectionText)

        rodata ++ out.toList ++ buildRoutines(needed.toSet)
    }

    // Maps support routine enum to function call
    private def buildRoutines(needed: Set[SupportRoutine]): List[x86Instr] =
        needed.toList.flatMap {
            case ExitRoutine        => exitRoutine
            case PrintStringRoutine => printStringRoutine
            case PrintIntRoutine    => printIntRoutine
            case PrintCharRoutine   => printCharRoutine
            case PrintRefRoutine    => printRefRoutine
            case PrintLnRoutine     => printLnRoutine
            case PrintBoolRoutine   => printBoolRoutine
            case ReadIntRoutine     => readIntRoutine
            case ReadCharRoutine    => readCharRoutine
            case OverflowRoutine    => overflowRoutine
            case DivZeroRoutine     => divZeroRoutine
            case NullDerefRoutine   => nullDerefRoutine
            case BoundsRoutine      => boundsRoutine
        }
    
    // Maps type of memory size enum to int
    private def mapToBytes(memSize: MemSize): Int = memSize match {
        case ByteM  => ByteSize
        case DwordM => DwordSize
        case QwordM => WordSize
    }

    // Maps ast types to memory size type enum
    private def getMemSize(ty: wacc.ast.SemType): MemSize = ty match {
        case wacc.ast.Type.BoolType => ByteM
        case wacc.ast.Type.CharType => ByteM
        case wacc.ast.Type.IntType  => DwordM
        case _                      => QwordM
    }

    // Lowers instructions using lowerer functions
    private def lowerInstr(instr: Instr, needed: Needed, stringLiterals: StringLiterals, frame: FrameState, out: ListBuffer[x86Instr])(using labeller: Labeller): Unit = instr match {
        case FunctionLabel(name)         => out ++= lowerFuncLabel(name)
        case EnterFrameI(params, locals) => lowerEnterFrame(params, locals, out)
        case LeaveFrameI                 => lowerLeaveFrame(out)
        case PushInt(_)                  => lowerPushI(instr, stringLiterals, out)
        case PushBool(_)                 => lowerPushI(instr, stringLiterals, out)
        case PushChar(_)                 => lowerPushI(instr, stringLiterals, out)
        case PushString(_)               => lowerPushI(instr, stringLiterals, out)
        case PushNull                    => lowerPushI(instr, stringLiterals, out)
        case ExitI                       => lowerExit(needed, out)
        case PrintI(ty, nl)              => lowerPrintI(ty, nl, needed, out)
        case ReadI(ty)                   => lowerReadI(ty, needed, out)
        case StoreVar(name)              => lowerStoreVar(name, frame, out)
        case LoadVar(name)               => lowerLoadVar(name, frame, out)
        case Label(id)                   => lowerLabel(id,out)
        case Jump(id)                    => lowerJump(id,out)
        case JumpIfFalse(id)             => lowerCondJump(id, out)
        case AddI                        => checkedArith(needed, Add(Regs.tmp0, Regs.tmp1), out)
        case SubI                        => checkedArith(needed, Sub(Regs.tmp0, Regs.tmp1), out)
        case MulI                        => checkedArith(needed, IMul(Regs.tmp0, Regs.tmp1), out)
        case DivI                        => checkedDiv(needed, Regs.tmp0, out)
        case ModI                        => checkedDiv(needed, Regs.arg3, out)
        case EqI                         => lowerComparison(EqualC, out)
        case NeI                         => lowerComparison(NotEqualC, out)
        case LtI                         => lowerComparison(LessC, out)
        case GtI                         => lowerComparison(GreaterC, out)
        case LeI                         => lowerComparison(LessEqC, out)
        case GeI                         => lowerComparison(GreaterEqC, out)
        case NotI                        => lowerNotI(out)
        case NegI                        => lowerNegI(needed, out)
        case LenI                        => lowerLenI(out)
        case OrdI                        => ()
        case ChrI                        => lowerChrI(needed, out)
        case CallI(name, argc)           => lowerCallI(name, argc, out)
        case ReturnI                     => lowerReturnI(out)
        case DupI                        => lowerDupI(out)
        case DropI                       => lowerDropI(out)
        case FreeI                       => lowerFreeI(needed, out)
        case NewArrayI(elemTy)           => lowerNewArrayI(elemTy, out)
        case ArrayLoadI(elemTy)          => lowerArrayLoadI(elemTy, needed, out)
        case ArrayStoreI(elemTy)         => lowerArrayStoreI(elemTy, needed, out)
        case LoadFstI                    => lowerLoadPair(needed, zeroOffset, out)
        case LoadSndI                    => lowerLoadPair(needed, WordSize, out)
        case StoreFstI                   => lowerStorePair(needed, zeroOffset, out)
        case StoreSndI                   => lowerStorePair(needed, WordSize, out)
        case NewPairI                    => lowerNewPairI(out)
    }

    // Dispatches push instructions to appropriate lowering function
    private def lowerPushI(instr: Instr, stringLiterals: StringLiterals, out: ListBuffer[x86Instr]): Unit = instr match {
        case PushInt(value)    => out += Push(Op.Imm(value))
        case PushBool(value)   => out += Push(Op.Imm(if value then trueBool else falseBool))
        case PushChar(value)   => out += Push(Op.Imm(value.toInt))
        case PushString(value) => lowerPushString(value, stringLiterals, out)
        case PushNull          => out += Push(Op.Imm(nullVal))
        case _                 => ()
    }

    // Converts lnoti to x86
    private def lowerNotI(out: ListBuffer[x86Instr]): Unit =
        out ++= List(Pop(Regs.tmp0), Xor(Regs.tmp0, Op.Imm(trueBool)), Push(Regs.tmp0))

    // Converts len to x86
    private def lowerLenI(out: ListBuffer[x86Instr]): Unit =
        out ++= List(Pop(Regs.tmp0), Mov(Regs.tmp0, Op.SizedMem(Regs.tmp0, zeroOffset, QwordM)), Push(Regs.tmp0))
    
    //Converts return to x86
    private def lowerReturnI(out: ListBuffer[x86Instr]): Unit =
        out += Pop(Regs.tmp0); lowerLeaveFrame(out)
        
    //Converts dup to x86
    private def lowerDupI(out: ListBuffer[x86Instr]): Unit =
        out ++= List(Pop(Regs.tmp0), Push(Regs.tmp0), Push(Regs.tmp0))

    //Converts drop to x86
    private def lowerDropI(out: ListBuffer[x86Instr]): Unit =
        out += Add(Regs.stackP, Op.Imm(WordSize))
    
    //Converts labels to x86Label
    private def lowerLabel(id: Int, out: ListBuffer[x86Instr]): Unit =
        out += LabelDef(LocalLabel(id))

    //Converts jump to x86jump
    private def lowerJump(id:Int, out: ListBuffer[x86Instr]): Unit = 
        out += Jmp(LocalLabel(id))

    // Converts func label to x86 label.
    private def lowerFuncLabel(name: String): List[x86Instr] =
        if name == "main" then List(LabelDef(NamedLabel(name)))
        else List(LabelDef(NamedLabel(s"wacc_$name")))

    // Converts print representation to x86 using routines
    private def lowerPrintI(ty: wacc.ast.SemType, nl: Boolean, needed: Needed, out: ListBuffer[x86Instr]): Unit = {
        val (routine, label) = ty match {
            case wacc.ast.Type.IntType    => (PrintIntRoutine,    Labels.printi)
            case wacc.ast.Type.BoolType   => (PrintBoolRoutine,   Labels.printb)
            case wacc.ast.Type.CharType   => (PrintCharRoutine,   Labels.printc)
            case wacc.ast.Type.StringType => (PrintStringRoutine, Labels.prints)
            case _                        => (PrintRefRoutine,    Labels.printp)
        }
        if nl then lowerPrintLn(needed, routine, label, out)
        else lowerPrint(needed, routine, label, out)
    }

    // Gets routines and formatting required for converting read to x86 based on type.
    private def lowerReadI(ty: wacc.ast.SemType, needed: Needed, out: ListBuffer[x86Instr])(using labeller: Labeller): Unit = {
        val (routine, fmt) = if ty == wacc.ast.Type.IntType
            then (ReadIntRoutine, Labels.readiFmt)
            else (ReadCharRoutine, Labels.readcFmt)
        lowerRead(needed, routine, fmt, out)
    }

    // Converts read to x86 instructions
    private def lowerRead(needed: Needed, routine: SupportRoutine, fmtLabel: NamedLabel, out: ListBuffer[x86Instr])(using labeller: Labeller): Unit = {
        needed += routine
        val skipLabel = NamedLabel(s".L._read_skip_${labeller.generate()}")
        out += Pop(Regs.tmp1)
        out += Mov(Op.SizedMem(Regs.stackP, zeroOffset, QwordM), Op.Imm(nullVal))
        out += Mov(Regs.arg2, Regs.stackP)
        out += Lea(Regs.arg1, fmtLabel)
        out += MovAl(nullVal)
        emitAlignedCall(Labels.scanf, out, PairSize)
        out += Mov(Regs.tmp2, Op.SizedMem(Regs.stackP, zeroOffset, QwordM))
        out += Cmp(Regs.ret32, Op.Imm(nullVal))
        out += Jmp(GreaterC, skipLabel)
        out += Mov(Regs.tmp2, Regs.tmp1)
        out += LabelDef(skipLabel)
        out += Push(Regs.tmp2)
    }

    // Converts print to x86 instr
    private def lowerPrint(needed: Needed, routine: SupportRoutine, label: NamedLabel, out: ListBuffer[x86Instr]): Unit = {
        needed += routine
        out += Pop(Regs.arg1)
        out += Call(label)
    }

    // Converts println to x86Instr using lowerPrint
    private def lowerPrintLn(needed: Needed, routine: SupportRoutine, label: NamedLabel, out: ListBuffer[x86Instr]): Unit = {
        needed += PrintLnRoutine
        lowerPrint(needed, routine, label, out)
        out += Call(Labels.println)
    }

    // Converts exit to x86 instr
    private def lowerExit(needed: Needed, out: ListBuffer[x86Instr]): Unit = {
        needed += ExitRoutine
        out += Pop(Regs.arg1)
        out += Call(Labels.exit)
    }

    // Converts pushstring to x86 instr
    private def lowerPushString(value: String, stringLiterals: StringLiterals, out: ListBuffer[x86Instr]): Unit = {
        val label = stringLiterals.getOrElseUpdate(value, s".L.str${stringLiterals.size}")
        out += Lea(Regs.tmp0, NamedLabel(label))
        out += Push(Regs.tmp0)
    }

    // Converts storevar to x86 instr
    private def lowerStoreVar(name: wacc.renamer.Renamed, frame: FrameState, out: ListBuffer[x86Instr]): Unit =
        val offset = frame.varOffsets(name)
        out += Pop(Regs.tmp0)
        out += Mov(Op.SizedMem(Regs.baseP, -offset, QwordM), Regs.tmp0)

    // Converts loadvar to x86 instr
    private def lowerLoadVar(name: wacc.renamer.Renamed, frame: FrameState, out: ListBuffer[x86Instr]): Unit = {
        val offset = frame.varOffsets(name)
        out += Mov(Regs.tmp0, Op.SizedMem(Regs.baseP, -offset, QwordM))
        out += Push(Regs.tmp0)
    }

    // Converts conditional jump to x86 instr
    private def lowerCondJump(id: Int, out: ListBuffer[x86Instr]): Unit = {
        out += Pop(Regs.tmp0)
        out += Cmp(Regs.tmp0, Op.Imm(nullVal))
        out += Jmp(EqualC, LocalLabel(id))
    }

    // Converts negation to x86 instr
    private def lowerNegI(needed: Needed, out: ListBuffer[x86Instr]): Unit = {
        requireErr(needed, OverflowRoutine)
        out += Pop(Regs.tmp0)
        out += Neg(Op.R(RAX))
        out += Mov(Regs.tmp2, Regs.tmp0)
        out += Cdqe
        out += Cmp(Regs.tmp0, Regs.tmp2)
        out += Jmp(NotEqualC, Labels.overflow)
        out += Push(Regs.tmp2)
    }

    // Converts Chr unary operator to x86 instr
    private def lowerChrI(needed: Needed, out: ListBuffer[x86Instr]): Unit = {
        requireErr(needed, BoundsRoutine)
        out += Pop(Regs.tmp0)
        out += Cmp(Regs.tmp0, Op.Imm(nullVal))
        out += Jmp(LessC, Labels.boundsNeg)
        out += Cmp(Regs.tmp0, Op.Imm(AsciiUpperBound))
        out += Jmp(GreaterEqC, Labels.boundsUp)
        out += Push(Regs.tmp0)
    }

    // Converts call to x86 instr
    private def lowerCallI(name: String, argc: Int, out: ListBuffer[x86Instr]): Unit = {
        out += Call(NamedLabel(s"wacc_$name"))
        out += Add(Regs.stackP, Op.Imm(argc * WordSize))
        out += Push(Regs.tmp0)
    }

    // Converts free to x86 instr
    private def lowerFreeI(needed: Needed, out: ListBuffer[x86Instr]): Unit = {
        requireErr(needed, NullDerefRoutine)
        out += Pop(Regs.arg1)
        out += Cmp(Regs.arg1, Op.Imm(nullVal))
        out += Jmp(EqualC, Labels.nullDeref)
        emitAlignedCall(Labels.free, out)
    }

    private def lowerNewArrayI(elemTy: wacc.ast.SemType, out: ListBuffer[x86Instr]): Unit =
        val memSize = getMemSize(elemTy)
        val byteAmt = mapToBytes(memSize)
        out += Pop(Regs.tmp1)                   
        out += Mov(Regs.tmp0, Regs.tmp1)              
        out += IMul(Regs.tmp0, Op.Imm(byteAmt))          
        out += Add(Regs.tmp0, Op.Imm(WordSize))       
        out += Mov(Regs.arg1, Regs.tmp0)
        emitAlignedCall(Labels.malloc, out)           
        out += Mov(Op.SizedMem(Regs.tmp0, zeroOffset, QwordM), Regs.tmp1)  
        out += Push(Regs.tmp0)    

    // Converts array loading to x86 instr
    private def lowerArrayLoadI(elemTy: wacc.ast.SemType, needed: Needed, out: ListBuffer[x86Instr]): Unit =
        val memSize = getMemSize(elemTy)
        val byteAmt = mapToBytes(memSize)
        requireErr(needed, BoundsRoutine)
        out += Pop(Regs.tmp0)  
        out += Pop(Regs.tmp1)   
        out += Cmp(Regs.tmp0, Op.Imm(nullVal))
        out += Jmp(LessC, Labels.boundsNeg)
        out += Cmp(Regs.tmp0, Op.SizedMem(Regs.tmp1, zeroOffset, QwordM))
        out += Jmp(GreaterEqC, Labels.boundsUp)
        out += IMul(Regs.tmp0, Op.Imm(byteAmt))
        out += Add(Regs.tmp1, Regs.tmp0)
        out += Add(Regs.tmp1, Op.Imm(WordSize))
        memSize match
            case ByteM  => out += Movzx(Regs.tmp0, Op.SizedMem(Op.R(RBX), zeroOffset, ByteM))
            case DwordM => out += Mov(Op.R(EAX), Op.SizedMem(Op.R(RBX), zeroOffset, DwordM))
            case QwordM => out += Mov(Regs.tmp0, Op.SizedMem(Op.R(RBX), zeroOffset, QwordM))
        out += Push(Regs.tmp0)

    // Converts array storing to x86 instr
    private def lowerArrayStoreI(elemTy: wacc.ast.SemType, needed: Needed, out: ListBuffer[x86Instr]): Unit =
        val memSize = getMemSize(elemTy)
        val byteAmt = mapToBytes(memSize)
        requireErr(needed, BoundsRoutine)
        out += Pop(Regs.tmp0)   
        out += Pop(Regs.tmp1)   
        out += Pop(Regs.tmp2)  
        out += Cmp(Regs.tmp1, Op.Imm(nullVal))
        out += Jmp(LessC, Labels.boundsNeg)
        out += Cmp(Regs.tmp1, Op.SizedMem(Regs.tmp2, zeroOffset, QwordM))
        out += Jmp(GreaterEqC, Labels.boundsUp)
        out += IMul(Regs.tmp1, Op.Imm(byteAmt))
        out += Add(Regs.tmp1, Regs.tmp2)
        out += Add(Regs.tmp1, Op.Imm(WordSize))
        memSize match
            case ByteM  => out += Mov(Op.SizedMem(Op.R(RBX), zeroOffset, ByteM),  Op.R(AL))
            case DwordM => out += Mov(Op.SizedMem(Op.R(RBX), zeroOffset, DwordM), Op.R(EAX))
            case QwordM => out += Mov(Op.SizedMem(Op.R(RBX), zeroOffset, QwordM), Regs.tmp0)

    // Converts pair laoding to x86 instr
    private def lowerLoadPair(needed: Needed, offset: Int, out: ListBuffer[x86Instr]): Unit = {
        requireErr(needed, NullDerefRoutine)
        out += Pop(Regs.tmp0)
        out += Cmp(Regs.tmp0, Op.Imm(nullVal))
        out += Jmp(EqualC, Labels.nullDeref)
        out += Mov(Regs.tmp1, Op.SizedMem(Regs.tmp0, offset, QwordM))
        out += Push(Regs.tmp1)
    }

    // Converts pair storing to x86 instr
    private def lowerStorePair(needed: Needed, offset: Int, out: ListBuffer[x86Instr]): Unit = {
        requireErr(needed, NullDerefRoutine)
        out += Pop(Regs.tmp0)
        out += Pop(Regs.tmp1)
        out += Cmp(Regs.tmp1, Op.Imm(nullVal))
        out += Jmp(EqualC, Labels.nullDeref)
        out += Mov(Op.SizedMem(Regs.tmp1, offset, QwordM), Regs.tmp0)
    }

    // Converts new pair creation to x86 instr
    private def lowerNewPairI(out: ListBuffer[x86Instr]): Unit = {
        out += Mov(Regs.arg1, Op.Imm(PairSize))
        emitAlignedCall(Labels.malloc, out)
        out += Pop(Regs.tmp1)
        out += Pop(Regs.tmp2)
        out += Mov(Op.SizedMem(Regs.tmp0, zeroOffset, QwordM), Regs.tmp2)
        out += Mov(Op.SizedMem(Regs.tmp0, WordSize, QwordM), Regs.tmp1)
        out += Push(Regs.tmp0)
    }

    // Converts frame entering to x86 instr
    private def lowerEnterFrame(params: Int, locals: Int, out: ListBuffer[x86Instr]): Unit = {
        val totalBytes = (params + locals) * WordSize
        out += Push(Regs.baseP)
        out += Mov(Regs.baseP, Regs.stackP)
        if totalBytes > zeroBytes then out += Sub(Regs.stackP, Op.Imm(totalBytes))
        (0 until params).reverse.foreach { paramCount =>
            out += Mov(Regs.tmp0, Op.SizedMem(Regs.baseP, PairSize + paramCount * WordSize, QwordM))
            out += Push(Regs.tmp0)
        }
    }

    // Converts frame leaving to x86 instr
    private def lowerLeaveFrame(out: ListBuffer[x86Instr]): Unit = {
        out += Mov(Regs.stackP, Regs.baseP)
        out += Pop(Regs.baseP)
        out += Ret
    }

    // Converts general comparison to x86 instr
    private def lowerComparison(cond: Condition, out: ListBuffer[x86Instr]): Unit = {
        out += Pop(Regs.tmp1)
        out += Pop(Regs.tmp0)
        out += Cmp(Regs.tmp0, Regs.tmp1)
        out += SetCC(cond, Regs.retLow)
        out += Movzx(Regs.tmp0, Regs.retLow)
        out += Push(Regs.tmp0)
    }

    // Converts general arithmetic operator to x86 instr
    private def checkedArith(needed: Needed, arithOp: x86Instr, out: ListBuffer[x86Instr]): Unit = {
        requireErr(needed, OverflowRoutine)
        out += Pop(Regs.tmp1)
        out += Pop(Regs.tmp0)
        out += arithOp
        out += Mov(Regs.tmp2, Regs.tmp0)
        out += Cdqe
        out += Cmp(Regs.tmp0, Regs.tmp2)
        out += Jmp(NotEqualC, Labels.overflow)
        out += Push(Regs.tmp2)
    }

    // Converts div/mod to x86 instr
    private def checkedDiv(needed: Needed, resultReg: Op, out: ListBuffer[x86Instr]): Unit = {
        requireErr(needed, DivZeroRoutine)
        out += Pop(Regs.tmp1)
        out += Pop(Regs.tmp0)
        out += Cmp(Regs.tmp1, Op.Imm(nullVal))
        out += Jmp(EqualC, Labels.divZero)
        out += Cqo
        out += IDiv(Op.R(RBX))
        out += Push(resultReg)
    }

    // Used to build x86 instr for stack alignment after label call
    private def emitAlignedCall(label: NamedLabel, out: ListBuffer[x86Instr], extraStack: Int = zeroBytes): Unit = {
        out += Push(Regs.baseP)
        out += Mov(Regs.baseP, Regs.stackP)
        out += And(Regs.stackP, Op.Imm(AlignMask))
        if extraStack > zeroBytes then out += Sub(Regs.stackP, Op.Imm(extraStack))
        out += Call(label)
        out += Mov(Regs.stackP, Regs.baseP)
        out += Pop(Regs.baseP)
    }

    // Used in exit routine, generating an immutable list to return
    private def alignedCall(label: NamedLabel, extraStack: Int = zeroBytes): List[x86Instr] = {
        val setup = List(Push(Regs.baseP), Mov(Regs.baseP, Regs.stackP),
            And(Regs.stackP, Op.Imm(AlignMask))) ++
            (if extraStack > zeroBytes then List(Sub(Regs.stackP, Op.Imm(extraStack))) else Nil)
        setup ++ List(Call(label), Mov(Regs.stackP, Regs.baseP), Pop(Regs.baseP))
    }

    // Adds relevant routines for error reporting
    private def requireErr(needed: Needed, routine: SupportRoutine): Unit = {
        needed += routine
        needed += PrintStringRoutine
        needed += ExitRoutine
    }

    // x86 for exit using aligned call
    private def exitRoutine =
        List(LabelDef(Labels.exit)) ++
        alignedCall(Labels.exitPlt) ++
        List(Ret)

    // general print routine that is used by printstr, printchr, printint, print ref.
    private def simplePrintRoutine(
        routineLabel: NamedLabel,
        fmtLabel:     NamedLabel,
        fmtStr:       String,
        setupArgs:    List[x86Instr]
    ): List[x86Instr] =
        List(
            SectionRodata,
            RodataInt(fmtStr.length),
            RodataString(fmtLabel, fmtStr),
            SectionText,
            LabelDef(routineLabel),
            Push(Regs.baseP),
            Mov(Regs.baseP, Regs.stackP),
            And(Regs.stackP, Op.Imm(AlignMask))
        ) ++ setupArgs ++ List(
            Lea(Regs.arg1, fmtLabel),
            MovAl(nullVal),
            Call(Labels.printf),
            Mov(Regs.arg1, Op.Imm(nullVal)),
            Call(Labels.fflush),
            Mov(Regs.stackP, Regs.baseP),
            Pop(Regs.baseP),
            Ret
        )

    // Convert print int to x86
    private def printIntRoutine: List[x86Instr] =
        simplePrintRoutine(Labels.printi, Labels.printiFmt, PrintFormat.int,
            List(Mov(Regs.arg2_32, Regs.arg1_32)))

    // Convert print char to x86
    private def printCharRoutine: List[x86Instr] =
        simplePrintRoutine(Labels.printc, Labels.printcFmt, PrintFormat.char,
            List(Mov(Regs.arg2_32, Regs.arg1_32)))

    // Convert print ref to x86
    private def printRefRoutine: List[x86Instr] =
        simplePrintRoutine(Labels.printp, Labels.printpFmt, PrintFormat.ref,
            List(Mov(Regs.arg2, Regs.arg1)))

    // Convert print string to x86
    private def printStringRoutine: List[x86Instr] =
        simplePrintRoutine(Labels.prints, Labels.printsFmt, PrintFormat.string,
            List(Mov(Regs.arg3, Regs.arg1), Mov(Regs.arg2_32, Op.SizedMem(Regs.arg1, -defaultTypeOffset, DwordM))))

    // Convert print ln to x86
    private def printLnRoutine: List[x86Instr] = List(
        SectionRodata,
        RodataInt(PrintFormat.newline.length),
        RodataString(Labels.printlnFmt, PrintFormat.newline),
        SectionText,
        LabelDef(Labels.println),
        Push(Regs.baseP),
        Mov(Regs.baseP, Regs.stackP),
        And(Regs.stackP, Op.Imm(AlignMask)),
        Lea(Regs.arg1, Labels.printlnFmt),
        MovAl(nullVal),
        Call(Labels.puts),
        Mov(Regs.arg1, Op.Imm(nullVal)),
        Call(Labels.fflush),
        Mov(Regs.stackP, Regs.baseP),
        Pop(Regs.baseP),
        Ret
    )

    // Convert print bool to x86
    private def printBoolRoutine: List[x86Instr] = List(
        SectionRodata,
        RodataInt(PrintFormat.boolTrue.length),
        RodataString(Labels.printbTrue, PrintFormat.boolTrue),
        RodataInt(PrintFormat.boolFalse.length),
        RodataString(Labels.printbFalse, PrintFormat.boolFalse),
        RodataInt(PrintFormat.string.length),
        RodataString(Labels.printbFmt, PrintFormat.string),
        SectionText,
        LabelDef(Labels.printb),
        Push(Regs.baseP),
        Mov(Regs.baseP, Regs.stackP),
        And(Regs.stackP, Op.Imm(AlignMask)),
        Cmp(Regs.arg1, Op.Imm(nullVal)),
        Lea(Regs.arg1, Labels.printbFalse),
        Jmp(EqualC, Labels.printbEnd),
        Lea(Regs.arg1, Labels.printbTrue),
        LabelDef(Labels.printbEnd),
        Mov(Regs.arg2_32, Op.SizedMem(Regs.arg1, -defaultTypeOffset, DwordM)),
        Mov(Regs.arg3, Regs.arg1),
        Lea(Regs.arg1, Labels.printbFmt),
        MovAl(nullVal),
        Call(Labels.printf),
        Mov(Regs.arg1, Op.Imm(nullVal)),
        Call(Labels.fflush),
        Mov(Regs.stackP, Regs.baseP),
        Pop(Regs.baseP),
        Ret
    )

    // General read routine used by other reads
    private def simpleReadRoutine(fmtLabel: NamedLabel, fmt: String): List[x86Instr] = List(
        SectionRodata,
        RodataInt(fmt.length),
        RodataString(fmtLabel, fmt),
        SectionText
    )

    // Convert read int and read char to x86
    private def readIntRoutine  = simpleReadRoutine(Labels.readiFmt, "%d")
    private def readCharRoutine = simpleReadRoutine(Labels.readcFmt, " %c")

    // Overflow error routine in x86
    private def overflowRoutine = errorRoutine(
        "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n",
        Labels.overflow, Labels.overflowMsg
    )

    // Divide by zero error routine in x86
    private def divZeroRoutine = errorRoutine(
        "DivideByZeroError: divide or modulo by zero.\n",
        Labels.divZero, Labels.divZeroMsg
    )

    // Dereferencing zero error routine in x86
    private def nullDerefRoutine = errorRoutine(
        "NullReferenceError: dereference a null reference.\n",
        Labels.nullDeref, Labels.nullDerefMsg
    )

    // Bounds routine in x86
    private def boundsRoutine: List[x86Instr] = {
        val msgUp  = "ArrayIndexOutOfBoundsError: index too large.\n"
        val msgNeg = "ArrayIndexOutOfBoundsError: negative index.\n"
        List(
            SectionRodata,
            RodataInt(msgNeg.length), RodataString(Labels.boundsNegMsg, msgNeg),
            RodataInt(msgUp.length),  RodataString(Labels.boundsUpMsg,  msgUp),
            SectionText
        ) ++
        boundsEntry(Labels.boundsNeg, Labels.boundsNegMsg) ++
        boundsEntry(Labels.boundsUp,  Labels.boundsUpMsg)
    }

    // Entry used in bounds routine
    private def boundsEntry(label: NamedLabel, msgLabel: NamedLabel): List[x86Instr] = List(
        LabelDef(label),
        Lea(Regs.arg1, msgLabel),
        Call(Labels.prints),
        Mov(Regs.arg1, Op.Imm(ErrExitCode)),
        Call(Labels.exit)
    )

    // General error routine x86 instr dependent on parameters.
    private def errorRoutine(msg: String, label: NamedLabel, msgLabel: NamedLabel): List[x86Instr] = List(
        SectionRodata, RodataInt(msg.length), RodataString(msgLabel, msg),
        SectionText, LabelDef(label),
        Lea(Regs.arg1, msgLabel), Call(Labels.prints),
        Mov(Regs.arg1, Op.Imm(ErrExitCode)), Call(Labels.exit)
    )
}