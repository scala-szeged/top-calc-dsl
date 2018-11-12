// See the OpalCalc demo: https://www.youtube.com/watch?time_continue=2&v=dIoeepnwP0Q

parse("No expression in the text")
parse("As easy as 1+1")
parse("As easy as 1+1 more text")


parse("5+5")
parse("2*100")
parse("2x100")
parse("ans")
parse("ans+1")


parse("hamster = 10")
parse("hamster = $10")
parse("5hamster")


parse("TODO: 5hamster+10%")
parse("MAYBE TODO: 5+5 4/2")


import scala.util.parsing.combinator.JavaTokenParsers

case class MyNumber(n: Double, unit: String) {
  def *(other: MyNumber) = MyNumber(n * other.n, unit)

  def /(other: MyNumber) = MyNumber(n / other.n, unit)

  def +(other: MyNumber) = MyNumber(n + other.n, unit)

  def -(other: MyNumber) = MyNumber(n - other.n, unit)
}

sealed trait Answer_? extends Product with Serializable

case class Answer(n: MyNumber, currency: Option[String]) extends Answer_? {
  override def toString: String = n.unit + n.n
}

case object NoAnswer extends Answer_?

var lastAnswer: Option[Answer] = None

def parse(str: String) = {
  println(str)
  lastAnswer = TopCalcDsl.parse(str)
  lastAnswer.getOrElse(NoAnswer)
}


object TopCalcDsl extends JavaTokenParsers {

  var variables: Map[String, MyNumber] = Map()


  def line: Parser[Option[Answer]] = rep(someAnswer | noAnswer) ^^ {
    list =>
      val oneOrZero = list.filter(_.nonEmpty)
      if (oneOrZero.nonEmpty)
        oneOrZero.head
      else
        None
  }

  def noAnswer =
    """[^\n]""".r ^^ {
      oneCharacter => None
    }

  def someAnswer: Parser[Some[Answer]] = varInit | answer(expr)

  def answer(p: Parser[MyNumber]): Parser[Some[Answer]] = p ^^ {
    number => Some(Answer(number, None))
  }

  def numberTimesVar = number("") ~ varRef ^^ {
    case number ~ variable => MyNumber(number.n * variable.n, variable.unit)
  }

  def varInit: Parser[Some[Answer]] = ident ~ "=" ~ expr ^^ {
    case v ~ _ ~ expr =>
      variables = variables.updated(v, expr)
      Some(Answer(expr, None))
  }

  def varRef: Parser[MyNumber] = ident ^? {
    case "ans" if lastAnswer.nonEmpty => lastAnswer.get.n
    case v if variables.contains(v) => variables(v)
  }

  def number(prefix: String): Parser[MyNumber] = prefix ~> floatingPointNumber ^^ {
    x => MyNumber(x.toDouble, prefix)
  }

  def factor: Parser[MyNumber] = numberTimesVar | varRef | number("$") | number("") | "(" ~> expr <~ ")"

  // 2 * 5 / 4
  def term: Parser[MyNumber] = factor ~ rep("x" ~ factor | "*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "x" ~ y) => x * y
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  // 5 + 6 - 7
  // 3 * 4 + 5
  def expr: Parser[MyNumber] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }


  def parse(source: String): Option[Answer] = parseAll(line, source) match {
    case Success(expression, _) =>
      expression

    case NoSuccess(err, next) =>
      println(err)
      throw new IllegalArgumentException("failed to parse " +
        "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
        err + "\n" + next.pos.longString)
  }
}
