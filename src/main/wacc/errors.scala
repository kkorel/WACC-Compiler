package wacc

import parsley.Parsley
import parsley.errors.{ErrorBuilder, Token}
import parsley.errors.tokenextractors.{LexToken, TillNextWhitespace}

sealed trait ErrorCategory {
    def name: String
}

object ErrorCategory {
    case object Syntax extends ErrorCategory { override def name: String = "Syntax Error" }
    case object Type extends ErrorCategory { override def name: String = "Type Error" }
    case object Scope extends ErrorCategory { override def name: String = "Scope Error" }
}

case class Pos(line: Int, col: Int) {
    def toTuple: (Int, Int) = (line, col)
}

sealed trait WACCError {
    def fileName: String
    def position: Pos
    def category: ErrorCategory
  
    def getFormatted(sourceLines: Array[String]): String = {
        val lineIdx = position.line - 1
        val caretPos = math.max(position.col - 1, 0)
        val errorLine = if (lineIdx >= 0 && lineIdx < sourceLines.length) sourceLines(lineIdx) else ""
        
        val context = new StringBuilder()
        val prefix = "  |"
        
        if (lineIdx > 0) context.append(s"$prefix${sourceLines(lineIdx - 1)}\n")
        context.append(s"$prefix$errorLine\n")
        context.append(s"$prefix${" " * caretPos}^\n")
        if (lineIdx + 1 < sourceLines.length) context.append(s"$prefix${sourceLines(lineIdx + 1)}\n")

        s"""${category.name} in $fileName (${position.line}, ${position.col}):
        |${headerInfo}
        |$context""".stripMargin
    }

    protected def headerInfo: String
}

case class SyntaxError(
    fileName: String,
    position: Pos,
    unexpectedRaw: String, 
    expected: List[String]
) extends WACCError {
  val category = ErrorCategory.Syntax
  private val parts = unexpectedRaw.split('|')
  val unexpected = parts.headOption.getOrElse("")
  val note = parts.lift(1).getOrElse("")

  override def headerInfo: String = {
    val exp = if (expected.isEmpty) "" else s"  expected ${expected.mkString(", ")}\n"
    val noteStr = if (note.isEmpty) "" else s"\nNote: $note"
    s"  unexpected $unexpected\n$exp$noteStr"
  }
}

case class SemanticError(
    fileName: String,
    position: Pos,
    category: ErrorCategory, 
    message: String
) extends WACCError {
  override def headerInfo: String = s"  $message"
}

class WACCErrorBuilder(fileName: String) extends ErrorBuilder[WACCError] with LexToken {
    
    override def tokens: Seq[Parsley[String]] = Seq(
        lexer.lexer.nonlexeme.integer.decimal.map(n => s"integer literal $n"),
        lexer.lexer.nonlexeme.names.identifier.map(name => s"identifier \"$name\"")
    ) ++ lexer.keywords.map(k => lexer.lexer.nonlexeme.symbol(k).as(s"keyword '$k'")) 
      ++ lexer.operators.map(op => lexer.lexer.nonlexeme.symbol(op).as(s"operator $op"))
    
    override def extractItem(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = {
        TillNextWhitespace.unexpectedToken(cs, amountOfInputParserWanted, _.isWhitespace)
    }

    type Position = Pos
    type Source = String
    type ErrorInfoLines = (String, List[String])
    type ExpectedItems = List[String]
    type Messages = List[String]
    type UnexpectedLine = Option[String]
    type ExpectedLine = Option[List[String]]
    type Message = String
    type LineInfo = (List[String], String, List[String], Int)

    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String

    override val numLinesBefore: Int = 1
    override val numLinesAfter: Int = 1

    override def pos(line: Int, col: Int): Position = Pos(line, col)
    override def source(sourceName: Option[String]): Source = sourceName.getOrElse("Unknown")
    
    override def raw(item: String): Raw = s"\"$item\""
    override def named(item: String): Named = item
    override val endOfInput: EndOfInput = "End of Input"

    override def message(msg: String): Message = msg
    override def reason(reason: String): Message = reason

    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        lineNum: Int,
        errorPointsAt: Int,
        errorWidth: Int
    ): LineInfo = (linesBefore.toList, line, linesAfter.toList, errorPointsAt)

    override def combineExpectedItems(items: Set[Item]): ExpectedItems = items.toList.distinct.sorted
    override def combineMessages(messages: Seq[Message]): Messages = messages.toList

    override def unexpected(unexpected: Option[Item]): UnexpectedLine = unexpected.map {
        case item if item.startsWith("\"") => s"raw input $item"
        case item => item
    }
    
    override def expected(expected: ExpectedItems): ExpectedLine = 
        if (expected.isEmpty) None else Some(expected)

    override def vanillaError(
        unexpected: UnexpectedLine,
        expected: ExpectedLine,
        reasons: Messages,
        lineInfo: LineInfo
    ): ErrorInfoLines = {
        val unexpectedStr = unexpected.getOrElse("")
        val expectedList = expected.getOrElse(Nil)
        val note = if (reasons.isEmpty) "" else reasons.mkString(", ")
        (s"$unexpectedStr|$note", expectedList)
    }

    override def specializedError(
        messages: Messages,
        lineInfo: LineInfo
    ): ErrorInfoLines = (messages.mkString("\n  "), Nil)

    override def build(
        position: Position,
        source: Source,
        errorInfoLines: ErrorInfoLines
    ): WACCError = {
        val (unexpectedWithReason, expected) = errorInfoLines
        SyntaxError(fileName, position, unexpectedWithReason, expected)
    }
}