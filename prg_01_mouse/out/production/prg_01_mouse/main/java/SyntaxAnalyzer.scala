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
  // syntax = syntax-rule { ´\n´ syntax-rule }
  private def parseSyntax() = {
    val tree = new Tree("syntax")
    tree.add(parseSyntaxRule())
    while (getLexeme().getToken() == Token.NEW_LINE) {
      val lexeme = getLexeme()
      tree.add(new Tree(lexeme.getLabel()))
      nextLexeme()
      tree.add(parseSyntaxRule())
    }
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
