package integration_test_backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.{Success, Failure}
import wacc._
import java.io.{File, ByteArrayInputStream}
import scala.sys.process._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import org.scalatest.BeforeAndAfterAll
import backend._

class WaccExamplesTests extends AnyFlatSpec with BeforeAndAfterAll {

  private var totalPassed = 0
  private var totalRun = 0

  private val examplesDir    = sys.env.getOrElse("WACC_EXAMPLES", "wacc_examples")
  private val gccPath        = sys.env.getOrElse("GCC_PATH", "/usr/bin/gcc")
  private val categoryResults = scala.collection.mutable.LinkedHashMap[String, String]()

  private lazy val allCategories =
    File(examplesDir).listFiles.toList
      .filter(f => f.isDirectory && !f.getName.startsWith("."))
      .flatMap(top => top.listFiles.toList
        .filter(f => f.isDirectory && !f.getName.startsWith("."))
        .map(sub => s"${top.getName}/${sub.getName}")
      )
      .filterNot(c => c == "invalid/whack" || c.startsWith("invalid/") || c == "valid/advanced")



  case class WaccTest(
    file:           File,
    source:         String,
    expectedOutput: String,
    expectedExit:   Int,
    input:          Option[String]
  )

  private def parseTestFile(file: File): Option[WaccTest] = {
    val lines = Source.fromFile(file).getLines().toList

    val expectedOutput = lines
      .dropWhile(_.trim != "# Output:")
      .drop(1)
      .takeWhile(l => l.trim != "# Program:" && l.trim != "# Exit:")
      .map(line => if line.trim == "#" then "" else line.stripPrefix("# "))
      .mkString("\n")
      .trim

    val exitCode = lines
      .dropWhile(_.trim != "# Exit:")
      .drop(1)
      .headOption
      .map(_.stripPrefix("# ").trim)
      .flatMap(_.toIntOption)
      .getOrElse(0)

    val input = lines
      .find(l => l.trim.startsWith("# Input:") || l.trim.startsWith("#Input:"))
      .map(_.replaceFirst("#\\s*Input:\\s*", "").replace(" ", "\n"))

    val source = lines
      .dropWhile(_.trim != "# Program:")
      .drop(1)
      .mkString("\n")
      .trim

    if source.isEmpty then None
    else Some(WaccTest(file, source, expectedOutput, exitCode, input))
  }


  private def compile(source: String): Either[Int, List[x86Instr]] =
      parser.parseFile(source) match
          case Failure(_) => Left(100)
          case Success(program) =>
              val (renamed, renameErrs) = renamer.rename(program)
              val (typed, typeErrs)     = typeChecker.check(renamed)
              if renameErrs.nonEmpty || typeErrs.nonEmpty then Left(200)
              else Right(x86Lowerer.lowerProgram(stackMachine.compileProgram(typed)))

  private def assembleAndRun(ir: List[x86Instr], input: Option[String]): (Int, String) = {
      val asmFile = File.createTempFile("wacc_test", ".s")
      val exeFile = File.createTempFile("wacc_test", "")

      try {
          x86Formatter.formatToFile(ir, asmFile)  // ← use formatToFile

          val gccResult =
              s"$gccPath -o ${exeFile.getAbsolutePath} -z noexecstack ${asmFile.getAbsolutePath}".!
          if gccResult != 0 then return (-1, "gcc failed")

          val output  = StringBuilder()
          val logger = ProcessLogger(
              out => output.append(out + "\n"),
              err => output.append(err + "\n")
          )
          val process = input match
              case Some(text) =>
                  Process(exeFile.getAbsolutePath) #< new ByteArrayInputStream(text.getBytes)
              case None =>
                  Process(exeFile.getAbsolutePath)

          val proc = process.run(logger)
          try {
              val exit = Await.result(Future(proc.exitValue()), 10.seconds)
              (exit, output.toString.trim)
          } catch {
              case _: java.util.concurrent.TimeoutException =>
                  proc.destroy()
                  (-1, "timeout: process exceeded 10s")
          }
      } finally {
          asmFile.delete()
          exeFile.delete()
      }
  }

  private def compileAndRun(source: String, input: Option[String]): (Int, String) =
      try compile(source) match
          case Left(code) => (code, "")
          case Right(ir)  => assembleAndRun(ir, input)  
      catch
          case e: IllegalArgumentException => (-2, s"not implemented: ${e.getMessage}")

  private def matchesOutput(actual: String, expected: String): Boolean =
    if expected.contains("#runtime_error#") then true
    else if expected.contains("#addrs#") then
      val pattern = java.util.regex.Pattern.quote(expected)
        .replace("#addrs#", "\\E0x[0-9a-fA-F]+\\Q")
      actual.matches(pattern)
    else actual == expected

  "basic examples"     should "pass" in { runCategory("valid/basic") }
  "sequence examples"  should "pass" in { runCategory("valid/sequence") }
  "IO examples"        should "pass" in { runCategory("valid/IO") }
  "variable examples"  should "pass" in { runCategory("valid/variables") }
  "expression examples" should "pass" in { runCategory("valid/expressions") }
  "array examples"     should "pass" in { runCategory("valid/array") }
  "if examples"        should "pass" in { runCategory("valid/if") }
  "while examples"     should "pass" in { runCategory("valid/while") }
  "scope examples"     should "pass" in { runCategory("valid/scope") }
  "function examples"  should "pass" in { runCategory("valid/function") }
  "pairs examples"     should "pass" in { runCategory("valid/pairs") }
  "runtimeErr examples" should "pass" in { runCategory("valid/runtimeErr") }

  private def runCategory(subdir: String): Unit = {
      val dir   = File(s"$examplesDir/$subdir")
      val files = collectFiles(dir)

      if files.isEmpty then
          cancel(s"No files found in $subdir")

      var passed = 0
      var failed = 0
      val failures = scala.collection.mutable.ListBuffer[String]()

      for file <- files do
          parseTestFile(file) match
              case None =>
              case Some(test) =>
                  val (actualExit, actualOutput) = compileAndRun(test.source, test.input)
                  if actualExit == -2 then
                      failed += 1
                      failures += s"  SKIP [${file.getName}]: ${actualOutput}"
                  else
                    if matchesOutput(actualOutput, test.expectedOutput) && actualExit == test.expectedExit then
                        passed += 1
                    else
                        failed += 1
                        val reason =
                            if actualExit != test.expectedExit
                            then s"exit: expected ${test.expectedExit} got $actualExit"
                            else s"output: expected '${test.expectedOutput}' got '$actualOutput'"
                        failures += s"  FAIL [${file.getName}]: $reason"

      val total = passed + failed
      val pct   = if total == 0 then 0 else (passed * 100) / total

      categoryResults(subdir) = s"$passed/$total ($pct%)"
      println(s"\n$subdir: $passed/$total ($pct%)")
      failures.foreach(println)

      totalPassed += passed
      totalRun += passed + failed
      failed shouldBe 0
  }

  private def collectFiles(dir: File): List[File] =
    if !dir.exists then Nil
    else dir.listFiles.toList.flatMap {
        case f if f.isDirectory               => collectFiles(f)
        case f if f.getName.endsWith(".wacc") => List(f)
        case _                                => Nil
    }

  override def afterAll(): Unit = {
    val pct = if totalRun == 0 then 0 else (totalPassed * 100) / totalRun

    println("\n==============================")
    println("Summary:")
     allCategories.foreach { cat =>
        val result = categoryResults.getOrElse(cat, "untested")
        val pad = " " * (25 - cat.length)
        println(s"  $cat$pad$result")
    }
    println("──────────────────────────────")
    println(s"  total                    $totalPassed/$totalRun ($pct%)")
    println("==============================")
  }
}