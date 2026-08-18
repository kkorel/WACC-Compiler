package wacc

import parsley.{Success, Failure}
import scala.io.Source
import backend.stackMachine
import backend.x86Lowerer
import backend.x86Formatter

def main(args: Array[String]): Unit = {
    args.headOption match {
        case None => 
            println("Usage: compile <filename>")
            System.exit(-1)
        
        case Some(filename) =>
            val fileContent = try {
                val source = Source.fromFile(filename)
                try source.mkString finally source.close()
            } catch {
                case e: Exception =>
                    println(s"Error Reading File: ${e.getMessage}")
                    System.exit(-1)
                    return
            }

            val sourceLines = fileContent.split('\n')

            parser.parseFile(fileContent, filename) match {
                case Failure(error: WACCError) =>
                    println(error.getFormatted(sourceLines))
                    System.exit(100) 
                
                case Success(program) =>
                    val allErrors = scala.collection.mutable.ListBuffer[SemanticError]()
                    
                    val (renamedProgram, renamerErrors) = renamer.rename(program)
                    allErrors ++= renamerErrors.map(_.toSemanticError(filename))
                    
                    val (typedProgram, typeErrors) = typeChecker.check(renamedProgram)
                    allErrors ++= typeErrors.map(_.toSemanticError(filename))
                    
                    if (allErrors.nonEmpty) {
                        allErrors.foreach { err =>
                            println(err.getFormatted(sourceLines))
                        }
                        System.exit(200)
                    }

                    val stackIR = stackMachine.compileProgram(typedProgram)
                    val x86IR = x86Lowerer.lowerProgram(stackIR)

                    val file = new java.io.File(filename).getName
                    val baseName = if (file.endsWith(".wacc")) file.stripSuffix(".wacc") else file
                    val outFile = baseName + ".s"

                    x86Formatter.formatToFile(x86IR, java.io.File(outFile))
                    
                    System.exit(0)
            }
    }
}