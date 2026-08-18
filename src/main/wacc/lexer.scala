package wacc

import parsley.Parsley
import parsley.token.{Lexer, Basic, Unicode}
import parsley.token.descriptions.*
import parsley.token.errors.*

object lexer {
    protected[wacc] val operators: Set[String] = Set(
        "+", "-",
        "*", "/", "%",
        ">", ">=", "<", "<=", 
        "==", "!=",
        "&&", "||", "!",
        "(", ")",
        "[", "]",
        "=",
        ";",
        ","
    )

    protected[wacc] val keywords: Set[String] = Set(
        "begin", "end",
        "if", "then", "else", "fi",
        "while", "do", "done",
        "int", "bool", "char", "string",
        "true", "false",
        "pair", "fst", "snd",
        "null",
        "is",
        "newpair",
        "call",
        "skip",
        "free",
        "read",
        "return",
        "exit",
        "print",
        "println",
        "len",
        "ord",
        "chr"
    ) 
    
    private val escapeCharacters: EscapeDesc = EscapeDesc.plain.copy(
        literals = Set('\'', '\"', '\\'),
        mapping = Map(
            "0" -> 0x00,
            "b" -> 0x08,
            "t" -> 0x09,
            "n" -> 0x0a,
            "f" -> 0x0c,
            "r" -> 0x0d
        )
    )

    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c == '_' || c.isLetter),
            identifierLetter = Basic(c => c == '_' || c.isLetterOrDigit)
        ),

        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = keywords,
            hardOperators = operators
        ),

        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
            lineCommentAllowsEOF = true
        ),

        textDesc = TextDesc.plain.copy(
        escapeSequences = escapeCharacters,
        graphicCharacter = Unicode(c =>
            c >= ' '.toInt &&
            c != '"'.toInt &&
            c != '\''.toInt &&
            c != '\\'.toInt)
        ),

        numericDesc = NumericDesc.plain.copy(
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false
        )
    )

    private object WaccErrorConfig extends ErrorConfig {
        
        // Character literals
        override def labelCharAscii = Label("character literal")
        
        // String literals
        override def labelStringAscii(multi: Boolean, raw: Boolean) = Label("string literal")
        
        // Integer literals (WACC only has signed decimal)
        override def labelIntegerSignedDecimal = Label("integer literal")
        
        override def labelNameIdentifier = "variable name"
        override def labelNameOperator = "operator"
        
        override def labelSpaceEndOfLineComment = Label("comment")
        
        override def verifiedCharBadCharsUsedInLiteral = BadCharsReason(Map(
            '\''.toInt -> "character literals cannot contain an unescaped single quote",
            '\"'.toInt -> "character literals cannot contain an unescaped double quote", 
            '\n'.toInt -> "character literals cannot contain a newline (use \\n)",
            '\t'.toInt -> "character literals cannot contain a tab (use \\t)",
            '\r'.toInt -> "character literals cannot contain a carriage return (use \\r)",
            '\f'.toInt -> "character literals cannot contain a form feed (use \\f)",
            '\b'.toInt -> "character literals cannot contain a backspace (use \\b)",
            '\u0000'.toInt -> "character literals cannot contain a null character (use \\0)",
            '\\'.toInt -> "character literals cannot contain an unescaped backslash (use \\\\)"
        ))
        
        override def verifiedStringBadCharsUsedInLiteral = BadCharsReason(Map(
            '\n'.toInt -> "string literals cannot contain a newline (use \\n)",
            '\r'.toInt -> "string literals cannot contain a carriage return (use \\r)",
            '\u0000'.toInt -> "string literals cannot contain null characters (use \\0)"
        ))
        
        override def labelSymbol = Map(
            // If statements
            "if" -> Label("if statement"),
            "then" -> LabelAndReason(
                reason = "if conditions must be followed by 'then'",
                label = "then"
            ),
            "else" -> LabelAndReason(
                reason = "all if statements must have an else clause",
                label = "else"
            ),
            "fi" -> LabelAndReason(
                reason = "if statement is not closed (missing 'fi')",
                label = "fi"
            ),
            
            // While loops
            "while" -> Label("while loop"),
            "do" -> LabelAndReason(
                reason = "while conditions must be followed by 'do'",
                label = "do"
            ),
            "done" -> LabelAndReason(
                reason = "while loop is not closed (missing 'done')",
                label = "done"
            ),
            
            // Scope management
            "begin" -> Label("begin block"),
            "end" -> LabelAndReason(
                reason = "a scope, function, or program body is not closed (missing 'end')",
                label = "end"
            ),
            
            // Functions
            "is" -> LabelAndReason(
                reason = "function declarations must use 'is' before the body",
                label = "is"
            ),
            "return" -> Label("return statement"),
            "call" -> Label("function call"),
            
            // Statements
            "skip" -> Label("skip statement"),
            "read" -> Label("read statement"),
            "free" -> Label("free statement"),
            "exit" -> Label("exit statement"),
            "print" -> Label("print statement"),
            "println" -> Label("println statement"),
            
            // Pairs
            "newpair" -> Label("pair creation"),
            "fst" -> Label("first element accessor"),
            "snd" -> Label("second element accessor"),
            "null" -> Label("null literal"),
            "pair" -> Label("pair type"),
            
            // Types
            "int" -> Label("int type"),
            "bool" -> Label("bool type"),
            "char" -> Label("char type"),
            "string" -> Label("string type"),
            
            // Boolean literals
            "true" -> Label("boolean literal"),
            "false" -> Label("boolean literal"),
            
            // Operators and punctuation
            "=" -> Label("assignment operator"),
            ";" -> LabelAndReason(
                reason = "statements must be separated by semicolons",
                label = "semicolon"
            ),
            "," -> Label("comma"),
            
            // Parentheses
            "(" -> Label("opening parenthesis"),
            ")" -> LabelAndReason(
                reason = "unmatched opening parenthesis",
                label = "closing parenthesis"
            ),
            
            // Brackets
            "[" -> Label("opening bracket"),
            "]" -> LabelAndReason(
                reason = "unmatched opening bracket or array index not closed",
                label = "closing bracket"
            ),
            
            // Binary operators
            "+" -> Label("binary operator"),
            "-" -> Label("binary operator"),
            "*" -> Label("binary operator"),
            "/" -> Label("binary operator"),
            "%" -> Label("binary operator"),
            ">" -> Label("binary operator"),
            ">=" -> Label("binary operator"),
            "<" -> Label("binary operator"),
            "<=" -> Label("binary operator"),
            "==" -> Label("binary operator"),
            "!=" -> Label("binary operator"),
            "&&" -> Label("binary operator"),
            "||" -> Label("binary operator"),
            
            // Unary operators
            "!" -> Label("unary operator"),
            "len" -> Label("unary operator"),
            "ord" -> Label("unary operator"),
            "chr" -> Label("unary operator"),
        )
    }

    protected[wacc] val lexer = Lexer(desc, WaccErrorConfig)

    val intLit: Parsley[Int] = lexer.lexeme.signed.decimal32
    val charLit: Parsley[Char] = lexer.lexeme.character.ascii
    val stringLit: Parsley[String] = lexer.lexeme.string.ascii
    val ident: Parsley[String] = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
