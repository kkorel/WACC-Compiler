package backend

/* 
x86Formatter:
    - converts ir representation of x86 to string
    - can convert to file (for  integration tests)
    - can convert to string (for unit tests)
    - Stage 3
*/
object x86Formatter {

    // abstracted out format logic for any writer
    def format(instrs: List[x86Instr], out: java.io.Writer): Unit =
        instrs.foreach { instr =>
            out.write(formatInstr(instr))
            out.write("\n")
        }

    // Builds file output incrementally
    def formatToFile(instrs: List[x86Instr], file: java.io.File): Unit =
        val writer = java.io.BufferedWriter(java.io.FileWriter(file))
        try format(instrs, writer)
        finally writer.close()

    // Outputs assembly to a string, keep for tests
    def formatToString(instrs: List[x86Instr]): String =
        val sw = java.io.StringWriter()
        format(instrs, sw)
        sw.toString

    // Formats instructions to string
    private def formatInstr(instr: x86Instr):String = instr match {

        //Header:
        case IntelSyntax => ".intel_syntax noprefix"
        case GloblMain => ".globl main"
        case SectionRodata => ".section .rodata"
        case SectionText => ".text"

        case LabelDef(label) => s"${formatLabel(label)}:"
        case Push(op) => s"\tpush ${formatOp(op)}"
        case Pop(op) => s"\tpop ${formatOp(op)}"
        case Mov(dest, src) => formatBin("mov", dest, src)
        case Add(dest, src) => formatBin("add", dest, src)
        case Sub(dest, src) => formatBin("sub", dest, src)
        case IMul(dest, src) => formatBin("imul", dest, src)
        case Xor(dest, src) => formatBin("xor", dest, src)
        case Movzx(dest, src) => formatBin("movzx", dest, src)
        case IDiv(Op.R(reg)) => s"\tidiv ${formatReg(reg)}"
        case Neg(Op.R(reg)) => s"\tneg ${formatReg(reg)}"
        case Cqo => "\tcqo"
        case Cdqe => "\tcdqe"
        case SetCC(cond, dest) => s"\t${condName(cond)} ${formatOp(dest)}"
        case And(dest, src) => formatBin("and", dest, src)
        case Call(NamedLabel(name)) => s"\tcall $name"
        case Ret => s"\tret"
        case Lea(dest, label)  => s"\tlea ${formatOp(dest)}, [rip + ${formatLabel(label)}]"
        case MovAl(value)         => s"\tmov al, $value"
        case RodataInt(value)     => s"\t.int $value"
        case RodataString(label, value) =>
            val escaped = value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\t", "\\t").replace("\b", "\\b").replace("\u0000", "\\0")
            s"${formatLabel(label)}:\n\t.asciz \"$escaped\""
        case Cmp(leftOp, rightOp)                     => formatBin("cmp", leftOp, rightOp)
        case Jmp(None, label)     => s"\tjmp ${formatLabel(label)}"
        case Jmp(Some(cond), label)  => s"\t${condToJmp(cond)} ${formatLabel(label)}"
    }

    // formats operands
    private def formatOp(op: Op): String = op match {
        case Op.Imm(value) => value.toString
        case Op.R(reg)   => formatReg(reg)
        case Op.SizedMem(Op.R(base), offset, ByteM)  => formatMem("byte ptr", base, offset)
        case Op.SizedMem(Op.R(base), offset, DwordM) => formatMem("dword ptr", base, offset)
        case Op.SizedMem(Op.R(base), offset, QwordM) => formatMem("qword ptr", base, offset)
    }

    // formats labels
    private def formatLabel(label: x86Label): String = label match
        case NamedLabel(name) => name
        case LocalLabel(id)  => s".L$id"

    // formats conditions
    private def condName(cond: Condition): String = cond match {
        case EqualC    => "sete"
        case NotEqualC => "setne"
        case LessC     => "setl"
        case GreaterC  => "setg"
        case LessEqC   => "setle"
        case GreaterEqC => "setge"
        case OverflowC => "seto"
    }

    // maps cond to correct jump instr
    private def condToJmp(cond: Condition): String = cond match {
        case EqualC     => "je"
        case NotEqualC  => "jne"
        case LessC      => "jl"
        case GreaterC   => "jg"
        case LessEqC    => "jle"
        case GreaterEqC => "jge"
        case OverflowC  => "jo"
    }

    // formats memory
    private def formatMem(size: String, base: x86Reg, offset: Int): String =
        if offset == zeroOffset then s"$size [${formatReg(base)}]"
        else if offset > zeroOffset then s"$size [${formatReg(base)} + $offset]"
        else s"$size [${formatReg(base)} - ${-offset}]"

    // formats binary instr
    private def formatBin(name: String, dst: Op, src: Op): String =
        s"\t$name ${formatOp(dst)}, ${formatOp(src)}"

    // formats registers using enum
    private def formatReg(r: x86Reg): String = r match {
        case RDI => "rdi"
        case RSP => "rsp"
        case RBP => "rbp"
        case RDX => "rdx"
        case RBX => "rbx"
        case RAX => "rax"
        case RSI => "rsi"
        case EAX => "eax"
        case ESI => "esi"
        case EDI => "edi"
        case AL  => "al"
        case RCX => "rcx"
    }
}