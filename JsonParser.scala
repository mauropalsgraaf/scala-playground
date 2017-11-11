import cats.Foldable
import cats.kernel.Monoid

import scala.util.parsing.combinator.RegexParsers

sealed trait JValue

case class JString(v: String) extends JValue

case class JInteger(v: Int) extends JValue

case class JObject(v: Map[String, JValue]) extends JValue

case class JArray(v: List[JValue]) extends JValue


object JsonParser extends RegexParsers {
  def jsonParser: Parser[JValue] = jsonObjectParser | jsonArrayParser | jsonStringParser | jsonIntParser

  def jsonStringParser: Parser[JString] = "\"" ~ """[a-zA-Z0-9]*""".r ~ "\"" ^^ { case _ ~ string ~ _ => JString(string) }

  def jsonObjectKeyParser: Parser[String] = jsonStringParser ^^ { case JString(x) => x }

  def jsonIntParser: Parser[JInteger] = """[0-9]*""".r ^^ { s => JInteger(s.toInt) }

  def jsonObjectParser: Parser[JObject] = "{" ~ repsep(jsonStringParser ~ ":" ~ jsonParser, ",") ~ "}" ^^ {
    case _ ~ xs ~ _ =>
      JObject(
        xs.foldLeft(Map.empty[String, JValue]) {
          (map, item) => {
            item match {
              case JString(s) ~ _ ~ v => map + (s -> v)
            }
          }
        }
      )
  }

  def jsonArrayParser: Parser[JArray] = "[" ~ repsep(jsonParser, ",") ~ "]" ^^ { case _ ~ list ~ _ => JArray(list) }

  def main(args: Array[String]): Unit = {
    import Visitor._
    import cats.instances.list._

    val json = scala.io.Source.fromFile("json.txt").mkString

    val x: JValue = JsonParser.parse(jsonParser, json).get

    println(
      Visitor.reduce(
        List(JString("4"), JString("9"))
      )
    )

    println(Visitor.sumListsFromJsonValue(x))
  }
}

object Visitor {

  import cats.instances.list._

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit def jIntegerMonoid(implicit intMonoid: Monoid[Int]): Monoid[JInteger] = new Monoid[JInteger] {
    def empty = JInteger(intMonoid.empty)

    def combine(x: JInteger, y: JInteger) = JInteger(intMonoid.combine(x.v, y.v))
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def empty = ""

    def combine(x: String, y: String): String = x + y
  }

  implicit def jStringMonoid(implicit stringMonoid: Monoid[String]): Monoid[JString] = new Monoid[JString] {
    def empty = JString(stringMonoid.empty)

    def combine(x: JString, y: JString): JString = JString(stringMonoid.combine(x.v, y.v))
  }

  def reduce[F[_], A](fa: F[A])(implicit foldable: Foldable[F], monoid: Monoid[A]): A = {
    foldable.foldLeft(fa, monoid.empty)(monoid.combine)
  }

  def sumListsFromJsonValue(jValue: JValue): JValue = jValue match {
    case v@JString(_) => v
    case v@JInteger(_) => v
    case JObject(map) => JObject(map.mapValues(sumListsFromJsonValue))
    case JArray(list) => {
      list match {
        case strings: List[JString] =>
          reduce(strings)
        case ints: List[JInteger] =>
          reduce(ints)
        case _ =>
          JArray(list)
      }
    }
  }
}
