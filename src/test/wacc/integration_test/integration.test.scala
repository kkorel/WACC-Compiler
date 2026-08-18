package integration_test_frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.{Success, Failure}
import wacc._
import java.io.File
import scala.io.Source

class IntegrationTests extends AnyFlatSpec {

    private val examplesDir = sys.env.getOrElse("WACC_EXAMPLES", "wacc_examples")

    def compile(source: String): Int = {
        parser.parseFile(source, "test") match {
            case Failure(_) => 100
            case Success(program) =>
                val (renamedProgram, renamerErrors) = renamer.rename(program)
                val (_, typeErrors) = typeChecker.check(renamedProgram)
                if (renamerErrors.nonEmpty || typeErrors.nonEmpty) 200 else 0
        }
    }

    it should "compile an empty program" in {
        compile("begin skip end") shouldBe 0
    }

    it should "handle basic variable declaration" in {
        compile("begin int x = 5 end") shouldBe 0
    }

    it should "work with multiple declarations" in {
        compile("begin int x = 1; int y = 2 end") shouldBe 0
    }

    it should "compile an if statement" in {
        compile("begin if true then skip else skip fi end") shouldBe 0
    }

    it should "compile a while statement" in {
        compile("begin while false do skip done end") shouldBe 0
    }

    it should "handle print" in {
        compile("""begin println "Hello" end""") shouldBe 0
    }

    it should "compile a simple function" in {
        compile("begin int f() is return 0 end skip end") shouldBe 0
    }

    it should "compile a function with parameters" in {
        compile("begin int add(int a, int b) is return a + b end skip end") shouldBe 0
    }

    it should "reject missing semicolon" in {
        compile("begin int x = 1 int y = 2 end") shouldBe 100
    }

    it should "fail on missing end" in {
        compile("begin int x = 1") shouldBe 100
    }

    it should "reject invalid type keyword" in {
        compile("begin invalid x = 1 end") shouldBe 100
    }

    it should "reject unmatched parentheses" in {
        compile("begin int x = (1 + 2 end") shouldBe 100
    }

    it should "reject missing fi in if statement" in {
        compile("begin if true then skip else skip end") shouldBe 100
    }

    it should "reject missing done in while statement" in {
        compile("begin while true do skip end") shouldBe 100
    }

    it should "catch undeclared variables" in {
        compile("begin x = 5 end") shouldBe 200
    }

    it should "reject type mismatch in declaration" in {
        compile("begin int x = true end") shouldBe 200
    }

    it should "fail on type mismatch in binary ops" in {
        compile("begin int x = 1 + true end") shouldBe 200
    }

    it should "reject non-bool condition in if" in {
        compile("begin if 1 then skip else skip fi end") shouldBe 200
    }

    it should "not allow return in main" in {
        compile("begin return 0 end") shouldBe 200
    }

    it should "reject return type mismatch" in {
        compile("begin int f() is return true end skip end") shouldBe 200
    }

    it should "reject undefined function call" in {
        compile("begin int x = call f() end") shouldBe 200
    }

    it should "reject wrong argument count" in {
        compile("begin int f(int a) is return a end int x = call f(1, 2) end") shouldBe 200
    }

    it should "reject free on non-heap type" in {
        compile("begin int x = 5; free x end") shouldBe 200
    }

    it should "catch redeclaration in same scope" in {
        compile("begin int x = 1; int x = 2 end") shouldBe 200
    }

    it should "compile nested scopes with shadowing" in {
        compile("begin int x = 1; begin int x = 2; int y = x end; int z = x end") shouldBe 0
    }

    it should "handle arrays" in {
        compile("begin int[] arr = [1, 2, 3]; int x = arr[0]; int y = len arr end") shouldBe 0
    }

    it should "handle pairs" in {
        compile("begin pair(int, bool) p = newpair(1, true); int x = fst p; bool y = snd p end") shouldBe 0
    }

    it should "compile function call" in {
        compile("begin int f(int n) is return n + 1 end int x = call f(5) end") shouldBe 0
    }

    it should "compile chained function calls" in {
        compile("begin int f(int n) is return n end int x = call f(5); int y = call f(x) end") shouldBe 0
    }

    it should "handle complex boolean expressions" in {
        compile("begin bool x = (1 + 2 * 3 > 5) && (10 / 2 <= 5 || true) end") shouldBe 0
    }

    // ── wacc_examples integration ──

    "syntax error examples"   should "pass" in { runCategory("invalid/syntaxErr", 100) }
    "semantic error examples" should "pass" in { runCategory("invalid/semanticErr", 200) }

    private def runCategory(subdir: String, expectedExit: Int): Unit = {
        val dir   = File(s"$examplesDir/$subdir")
        val files = collectFiles(dir)

        if files.isEmpty then cancel(s"No files found in $subdir")

        var passed  = 0
        var failed  = 0
        val failures = scala.collection.mutable.ListBuffer[String]()

        for file <- files do
            val source = Source.fromFile(file).mkString
            val result = compile(source)
            if result == expectedExit then passed += 1
            else
                failed += 1
                failures += s"  FAIL [${file.getName}]: expected exit $expectedExit got $result"

        val total = passed + failed
        val pct   = if total == 0 then 0 else (passed * 100) / total
        println(s"\n$subdir: $passed/$total ($pct%)")
        failures.foreach(println)

        failed shouldBe 0
    }

    private def collectFiles(dir: File): List[File] =
        if !dir.exists then Nil
        else dir.listFiles.toList.flatMap {
            case f if f.isDirectory               => collectFiles(f)
            case f if f.getName.endsWith(".wacc") => List(f)
            case _                                => Nil
        }
}