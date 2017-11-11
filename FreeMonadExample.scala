package org.free.example

import cats.{Id, ~>}
import cats.free.Free
import com.petsupplies.examples.Console._
import scala.io.StdIn

sealed trait ConsoleA[T]
final case class ReadLine() extends ConsoleA[String]
final case class PrintLine(value: String) extends ConsoleA[Unit]

object Console {
  type Console[A] = Free[ConsoleA, A]

  def readLine(): Free[ConsoleA, String] = Free.liftF(ReadLine())
  def printLine(value: String): Free[ConsoleA, Unit] = Free.liftF(PrintLine(value))
}

object Main {
  val program: Console[Unit] = for {
    _ <- printLine("Type something you feel like, maybe your current mood?")
    x <- readLine()
    _ <- printLine(s"You just typed $x in the console")
    _ <- printLine("and I'm going to print it again for you")
    _ <- printLine(x)
  } yield ()

  val interpreter: ConsoleA ~> Id =
    new (ConsoleA ~> Id) {
      def apply[A](consoleAction: ConsoleA[A]): Id[A] = consoleAction match {
        case ReadLine() =>
          StdIn.readLine(): Id[String]

        case PrintLine(x) =>
          println(x)
          ()
      }
    }
  
  def main(args: Array[String]): Unit = {
    program.foldMap(interpreter)
  }
}
