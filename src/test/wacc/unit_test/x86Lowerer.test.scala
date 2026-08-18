package unit_test

import parsley.{Success, Failure}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import backend._
import wacc.renamer
import wacc.typeChecker
import wacc.parser
import java.io.{File, PrintWriter}
import scala.sys.process._

class X86LowererTests extends AnyFlatSpec {

    private val gccPath = sys.env.getOrElse("GCC_PATH", "/usr/bin/gcc")
    private def runWacc(source: String, debug: Boolean = false): (Int, String) =
        parser.parseFile(source) match
            case Failure(_) => (100, "syntax error")
            case Success(program) =>
                val (renamed, renameErrs) = renamer.rename(program)
                val (typed, typeErrs)     = typeChecker.check(renamed)
                if renameErrs.nonEmpty || typeErrs.nonEmpty then (200, "semantic error")
                else
                    val ir  = x86Lowerer.lowerProgram(stackMachine.compileProgram(typed))
                    val asm = x86Formatter.formatToString(ir)
                    if debug then println(asm)
                    assembleAndRun(asm)

    private def assembleAndRun(asm: String): (Int, String) = {
        val asmFile = File.createTempFile("wacc_unit", ".s")
        val exeFile = File.createTempFile("wacc_unit", "")

        try {
            val pw = PrintWriter(asmFile)
            pw.write(asm)
            pw.close()

            if s"$gccPath -o ${exeFile.getAbsolutePath} -z noexecstack ${asmFile.getAbsolutePath}".! != 0
            then return (-1, "gcc failed")

            val output = StringBuilder()
            val code   = exeFile.getAbsolutePath.!(ProcessLogger(
                out => output.append(out + "\n"),
                err => output.append(err + "\n")
            ))
            (code, output.toString.trim)
        } finally {
            asmFile.delete()
            exeFile.delete()
        }
    }


    "x86Formatter" should "format Ret" in {
        x86Formatter.formatToString(List(Ret)) shouldBe "\tret\n"
    }

    "x86Formatter" should "format Mov reg reg" in {
        x86Formatter.formatToString(List(Mov(Op.R(RAX), Regs.baseP))) shouldBe "\tmov rax, rbp\n"
    }

    "x86Formatter" should "format Mov reg imm" in {
        x86Formatter.formatToString(List(Mov(Op.R(RAX), Op.Imm(0)))) shouldBe "\tmov rax, 0\n"
    }

    "x86Formatter" should "format Push imm" in {
        x86Formatter.formatToString(List(Push(Op.Imm(42)))) shouldBe "\tpush 42\n"
    }

    "x86Formatter" should "format Push reg" in {
        x86Formatter.formatToString(List(Push(Regs.baseP))) shouldBe "\tpush rbp\n"
    }

    "x86Formatter" should "format Pop" in {
        x86Formatter.formatToString(List(Pop(Regs.arg1))) shouldBe "\tpop rdi\n"
    }

    "x86Formatter" should "format Call" in {
        x86Formatter.formatToString(List(Call(Labels.exit))) shouldBe "\tcall _exit\n"
    }

    "x86Formatter" should "format LabelDef named" in {
        x86Formatter.formatToString(List(LabelDef(NamedLabel("main")))) shouldBe "main:\n"
    }

    "x86Formatter" should "format LabelDef local" in {
        x86Formatter.formatToString(List(LabelDef(LocalLabel(0)))) shouldBe ".L0:\n"
    }

    "x86Formatter" should "format And" in {
        x86Formatter.formatToString(List(And(Op.R(RSP), Op.Imm(-16)))) shouldBe "\tand rsp, -16\n"
    }

    "x86Formatter" should "format Sub" in {
        x86Formatter.formatToString(List(Sub(Op.R(RSP), Op.Imm(8)))) shouldBe "\tsub rsp, 8\n"
    }

    "x86Formatter" should "format Add" in {
        x86Formatter.formatToString(List(Add(Op.R(RAX), Op.Imm(1)))) shouldBe "\tadd rax, 1\n"
    }

    "x86Formatter" should "format IMul" in {
        x86Formatter.formatToString(List(IMul(Op.R(RAX), Op.R(RCX)))) shouldBe "\timul rax, rcx\n"
    }

    "x86Formatter" should "format IDiv" in {
        x86Formatter.formatToString(List(IDiv(Op.R(RCX)))) shouldBe "\tidiv rcx\n"
    }

    "x86Formatter" should "format Neg" in {
        x86Formatter.formatToString(List(Neg(Op.R(RAX)))) shouldBe "\tneg rax\n"
    }

    "x86Formatter" should "format Cqo" in {
        x86Formatter.formatToString(List(Cqo)) shouldBe "\tcqo\n"
    }

    "x86Formatter" should "format Xor" in {
        x86Formatter.formatToString(List(Xor(Op.R(RAX), Op.R(RAX)))) shouldBe "\txor rax, rax\n"
    }

    "x86Formatter" should "format Movzx" in {
        x86Formatter.formatToString(List(Movzx(Op.R(RAX), Op.R(AL)))) shouldBe "\tmovzx rax, al\n"
    }

    "x86Formatter" should "format SetCC equal" in {
        x86Formatter.formatToString(List(SetCC(EqualC, Op.R(AL)))) shouldBe "\tsete al\n"
    }

    "x86Formatter" should "format SetCC less" in {
        x86Formatter.formatToString(List(SetCC(LessC, Op.R(AL)))) shouldBe "\tsetl al\n"
    }

    "x86Formatter" should "format Cmp" in {
        x86Formatter.formatToString(List(Cmp(Op.R(RAX), Op.Imm(0)))) shouldBe "\tcmp rax, 0\n"
    }

    "x86Formatter" should "format Jmp unconditional" in {
        x86Formatter.formatToString(List(Jmp(None, LocalLabel(0)))) shouldBe "\tjmp .L0\n"
    }

    "x86Formatter" should "format Jmp equal" in {
        x86Formatter.formatToString(List(Jmp(Some(EqualC), LocalLabel(1)))) shouldBe "\tje .L1\n"
    }

    "x86Formatter" should "format Jmp not equal" in {
        x86Formatter.formatToString(List(Jmp(Some(NotEqualC), LocalLabel(2)))) shouldBe "\tjne .L2\n"
    }

    "x86Formatter" should "format Jmp less" in {
        x86Formatter.formatToString(List(Jmp(Some(LessC), LocalLabel(3)))) shouldBe "\tjl .L3\n"
    }

    "x86Formatter" should "format Jmp greater" in {
        x86Formatter.formatToString(List(Jmp(Some(GreaterC), LocalLabel(4)))) shouldBe "\tjg .L4\n"
    }

    "x86Formatter" should "format Lea" in {
        x86Formatter.formatToString(List(Lea(Op.R(RAX), NamedLabel("foo")))) shouldBe "\tlea rax, [rip + foo]\n"
    }

    "x86Formatter" should "format MovAl" in {
        x86Formatter.formatToString(List(MovAl(0))) shouldBe "\tmov al, 0\n"
    }

    "x86Formatter" should "format Directive" in {
        x86Formatter.formatToString(List(SectionText)) shouldBe ".text\n"
    }

    "x86Formatter" should "format RodataInt" in {
        x86Formatter.formatToString(List(RodataInt(5))) shouldBe "\t.int 5\n"
    }

    "x86Formatter" should "format RodataString" in {
        x86Formatter.formatToString(List(RodataString(NamedLabel("foo"), "hello"))) shouldBe "foo:\n\t.asciz \"hello\"\n"
    }

    "x86Formatter" should "format Mem with positive offset" in {
        x86Formatter.formatToString(List(Mov(Op.R(RAX), Op.SizedMem(Regs.baseP, 8, QwordM)))) shouldBe "\tmov rax, qword ptr [rbp + 8]\n"
    }

    "x86Formatter" should "format Mem with negative offset" in {
        x86Formatter.formatToString(List(Mov(Op.R(RAX), Op.SizedMem(Regs.baseP, -8, QwordM)))) shouldBe "\tmov rax, qword ptr [rbp - 8]\n"
    }

    "x86Formatter" should "format SizedMem" in {
        x86Formatter.formatToString(List(Mov(Op.R(ESI), Op.SizedMem(Regs.arg1, -4, DwordM)))) shouldBe "\tmov esi, dword ptr [rdi - 4]\n"
    }

    "x86Formatter" should "format multiple instructions" in {
        x86Formatter.formatToString(List(Push(Regs.baseP), Ret)) shouldBe "\tpush rbp\n\tret\n"
    }

    "ExitI" should "exit with code 255" in {
        val (code, _) = runWacc("begin exit 255 end")
        code shouldBe 255
    }

    "PrintStringI" should "print hello" in {
        val (code, output) = runWacc("""begin print "hello" end""")
        code   shouldBe 0
        output shouldBe "hello"
    }

    "PrintIntI" should "print 42" in {
        val (code, output) = runWacc("begin print 42 end")
        code   shouldBe 0
        output shouldBe "42"
    }

    "PrintBoolI" should "print true" in {
        val (code, output) = runWacc("begin print true end")
        code   shouldBe 0
        output shouldBe "true"
    }

    "PrintCharI" should "print a" in {
        val (code, output) = runWacc("begin print 'a' end")
        code   shouldBe 0
        output shouldBe "a"
    }

    "PrintCharArray" should "print s" in {
        val (code, output) = runWacc("begin char[] s = ['h','i','!']; println s end")
        code   shouldBe 0
        output shouldBe "hi!"
    }

    "PrintArrayNested" should "print" in {
        val (code, output) = runWacc("""begin
            int[] a = [1,2,3];
            int[] b = [3,4];
            int[][] c = [a,b] ;
            println c[0][2] ;
            println c[1][0]
            end """)
        code shouldBe 0
        output shouldBe "3\n3"
    }
}