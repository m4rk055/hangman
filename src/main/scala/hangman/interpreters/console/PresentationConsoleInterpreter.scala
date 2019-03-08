package hangman.interpreters.console

import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import hangman.domain.algebra._
import hangman.domain.model._

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

class PresentationConsoleInterpreter[F[_]: Concurrent: Timer: Sync]() extends PresentationAlgebra[F] {

  override def startApp: F[Unit] = {

    def drawLoading(s: String): F[Unit] =
      clear >>
        Sync[F].delay(println("Loading" + s)) >>
        Timer[F].sleep(FiniteDuration(100, MILLISECONDS)) >>
        drawLoading(if (s == ".") ".." else if (s == "..") "..." else "." )

    Concurrent[F].race(
      Timer[F].sleep(FiniteDuration(1500, MILLISECONDS)),
      drawLoading(".")
    ) >> Timer[F].sleep(FiniteDuration(200, MILLISECONDS)) >> clear
  }

  override def drawWord(word: Word, guesses: Set[Guess]): F[Unit] =
    Sync[F].delay(
      println(word.value.map(c => if (guesses.exists(_.value == c)) c.toUpper else '_').mkString_(" ")))

  override def drawGuesses(word: Word, guesses: Set[Guess]): F[Unit] =
    Sync[F].delay(
      println(s"Guessed: ${guesses.filterNot(g => word contains g.value).map(_.value.toUpper).toList.mkString_(", ")}"))

  override def drawHangedMan(hanged: Set[BodyPart]): F[Unit] =
    Sync[F].delay {

      def draw(head: String, body: String, leftArm: String, rightArm: String, leftLeg: String, rightLeg: String) =
        s"""
           |___
           ||  |
           ||  $head
           || $leftArm$body$rightArm
           || $leftLeg $rightLeg
           |-
         """.stripMargin

      case class Man(head: String, body: String, leftArm: String, rightArm: String, leftLeg: String, rightLeg: String)

      val man =
        hanged.foldLeft(Man(" ", " ", " ", " ", " ", " "))((a, c) =>
          c match {
            case Head => a.copy(head= "o")
            case Body => a.copy(body= "|")
            case LeftArm => a.copy(leftArm= "/")
            case RightArm => a.copy(rightArm = """\""")
            case LeftLeg => a.copy(leftLeg = "/")
            case RightLeg => a.copy(rightLeg = """\""")
          }
        )

      println(draw(man.head, man.body, man.leftArm, man.rightArm, man.leftLeg, man.rightLeg))
    }

  override def drawEndGame(game: Game): F[Unit] =
    Sync[F].delay {
      game.gameState match {
        case Initializing => ()
        case Initialized(_, _, _, state) =>
          state match {
            case ChallengerWon => println("Challenger won")
            case GuesserWon => println("Guesser won")
            case _ => ()
          }
      }
    }

  override def clear: F[Unit] = {
    import scala.sys.process._
    Sync[F].delay("clear".!)
  }

  override def drawUninitializedGame(game: Game): F[Unit] =
    Sync[F].delay(println("Game not initialized"))
}
