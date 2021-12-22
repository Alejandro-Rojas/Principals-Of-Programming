/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s):
 * Description: Prg 01 - SyntaxAnalyzer (an iterable syntax analyzer)
 */

/*
mouse       = { statement } ´$$´
statement   = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
string      = ´"´ { character } ´"´
identifier  = letter
literal     = ´0´ | nonzero { digit }
nonzero     = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
if          = ´[´ { statement } ´]´
while       = ´(´ { statement } ´)´
letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´x´ | ´y´ | ´w´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´X´ | ´Y´ | ´W´ | ´Z´
punctuation = ´.´ | ´,´ | ´;´ | ´:´ | ´?´ | ´!´
special     = ´<´ | ´_´ | ´@´ | ´#´ | ´$´ | ´%´ | ´^´ | ´&´ | ´(´ | ´)´ | ´-´ | ´+´ | ´=´ | ´'´ | ´/´ | ´\´ | ´[´ | ´]´ | ´{´ | ´}´ | ´|´
blank       = ´ ´
character   = letter | digit | punctuation | special | blank
 */

class SyntaxAnalyzer(private var source: String) {

  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null

  // returns the current lexeme
  private def getLexeme(): Lexeme = {
    if (current == null) {
      current = it.next
    }
    //    println(current)
    current
  }

  // advances the input one lexeme
  private def nextLexeme() = {
    current = it.next
  }

  // TODO: finish the recursive descent parser
  // parses the program, returning its corresponding parse tree
  def parse() = {
    parseMouse()
  }

  private def parseMouse() = {
    val tree = new Tree("mouse")
    tree.add(parseStmnt())
    while (getLexeme().getToken() == Token.EO_PRG) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseStmnt())
    }
    tree
  }

  private def parseStatement() = {
    val tree = new Tree("statement")
    val lexeme = getLexeme()
    if (lexeme.getToken() == Token.OPEN_BRACKET)
      tree.add(parseIf())
    else if (lexeme.getToken() == Token.OPEN_PAR)
      tree.add(parseWhile())
    else if (lexeme.getToken() == Token.EO_PRG)
      tree.add(parseMouse())
    else if (lexeme.getToken() == Token.IF || lexeme.getToken() == Token.WHILE) {
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
    }
    else
      throw new Exception("Syntax error: '[', '{', '(', meta-identifier, or terminal-string expected!")
    tree
  }

  // statement = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
  private def parseStmnt() = {
    val tree = new Tree("statement")
    tree.add(parseString())
    var done = false
    while (!done) {
      val lexeme = getLexeme()
      if (lexeme.getToken() != Token.READ_PUSH &&
        lexeme.getToken() != Token.POPS_OUTPUT &&
        lexeme.getToken() != Token.POPS_OUTPUT &&
        lexeme.getToken() != Token.POP_STACKS &&
        lexeme.getToken() != Token.ADD_STACKS &&
        lexeme.getToken() != Token.SUB_STACKS &&
        lexeme.getToken() != Token.MULT_STACKS &&
        lexeme.getToken() != Token.DIV_STACKS &&
        lexeme.getToken() != Token.Remain_Stacks &&
        lexeme.getToken() != Token.RETRIEV_VALUE &&
        lexeme.getToken() != Token.BREAK_LOOP &&
        lexeme.getToken() != Token.STRING &&
        lexeme.getToken() != Token.IDENTIFIER &&
        lexeme.getToken() != Token.LITERAL &&
        lexeme.getToken() != Token.IF &&
        lexeme.getToken() != Token.WHILE)
        done = true
      else
        tree.add(parseString())
    }
    tree
  }

  private def parseString() = {
    val tree = new Tree("string")
    tree.add(parseIdentifier())
    while (getLexeme().getToken() == Token.QUOTES) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseChar())
    }
    tree
  }

  // identifier  = letter
  def parseIdentifier(): Tree = {
    val tree = new Tree("identifer")
    tree.add(parseLiteral())
    while (getLexeme().getToken() == Token.IDENTIFIER) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseLetter())
    }
    tree
  }

  // identifier  = letter
  def parseLetter(): Tree = {
    val tree = new Tree("letter")
    tree.add(parseLiteral())
    while (getLexeme().getToken() == Token.LETTER) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseLiteral())
    }
    tree
  }

  // literal     = ´0´ | nonzero { digit }
  def parseLiteral(): Tree = {
    val tree = new Tree("literal")
    tree.add(parseNoneZero())
    while (getLexeme().getToken() == Token.LITERAL) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseNoneZero())
    }
    tree
  }

  // nonzero     = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8
  def parseNoneZero(): Tree = {
    val tree = new Tree("nonezero")
    tree.add(parseDigit())
    while (getLexeme().getToken() == Token.NONZERO) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseDigit())
    }
    tree
  }

  // digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ |
  def parseDigit(): Tree = {
    val tree = new Tree("digit")
    tree.add(parseLiteral())
    while (getLexeme().getToken() == Token.DIGIT) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseIf())
    }
    tree
  }

  // if    = ´[´ { statement } ´]´
  private def parseIf() = {
    val tree = new Tree("if")
    var lexeme = getLexeme()
    if (lexeme.getToken() == Token.OPEN_BRACKET) {
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseStmnt())
      lexeme = getLexeme()
      if (lexeme.getToken() == Token.CLOSE_BRACKET) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
      }
      else
        throw new Exception("Syntax error: ']' expected!")
    }
    else
      throw new Exception("Syntax error: '[' expected!")
    tree
  }


    // while       = ´(´ { statement } ´)´
    def parseWhile() = {
      val tree = new Tree("while")
      var lexeme = getLexeme()
      if (lexeme.getToken() == Token.OPEN_PAR) {
        tree.add(new Tree(lexeme.getLabel()))
        nextLexeme()
        tree.add(branch = parseStmnt())
        lexeme = getLexeme()
        if (lexeme.getToken() == Token.CLOSE_PAR) {
          tree.add(new Tree(lexeme.getLabel()))
          nextLexeme()
        }
        else
          throw new Exception("Syntax error: ')' expected!")
      }
      else
        throw new Exception("Syntax error: '(' expected!")
      tree
    }
  // Punctuation = ´.´ | ´,´ | ´;´ | ´:´ | ´?´ | ´!´
  def parsePunctuation(): Tree = {
    val tree = new Tree("Punctuaton")
    tree.add(parseDigit())
    while (getLexeme().getToken() == Token.PUNCTUATIONS) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseStmnt())
    }
    tree
  }
  // special     = ´<´ | ´_´ | ´@´ | ´#´ | ´$´ | ´%´
  def parseSpecial(): Tree = {
    val tree = new Tree("Special")
    tree.add(parseDigit())
    while (getLexeme().getToken() == Token.SPECIAL) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseStmnt())
    }
    tree
  }

  // blank       = ´ ´
  def parseBlank(): Tree = {
    val tree = new Tree("blank")
    tree.add(parseDigit())
    while (getLexeme().getToken() == Token.BLANK) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseStmnt())
    }
    tree
  }


  // character   = letter | digit | punctuation | special | blank
  private def parseChar() = {
    val tree = new Tree("term")
    val lexeme = getLexeme()
    if (lexeme.getToken() == Token.LETTER)
      tree.add(parseLetter())
    else if (lexeme.getToken() == Token.DIGIT)
      tree.add(parseDigit())
    else if (lexeme.getToken() == Token.PUNCTUATIONS)
      tree.add(parsePunctuation())
    else if (lexeme.getToken() == Token.SPECIAL)
      tree.add(parseSpecial())
    else
      throw new Exception("Syntax error: Character")
    tree
  }

}



  object SyntaxAnalyzer {
    def main(args: Array[String]): Unit = {

      // check if source file was passed through the command-line
      if (args.length != 1) {
        print("Missing source file!")
        System.exit(1)
      }

      val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
      val parseTree = syntaxAnalyzer.parse()
      print(parseTree)
    }
  }
