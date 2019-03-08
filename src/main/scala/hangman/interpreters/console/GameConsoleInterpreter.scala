package hangman.interpreters.console

import cats.effect.Sync
import cats.implicits._
import hangman.Util._
import hangman.domain.algebra._
import hangman.domain.model._

import scala.util.{Failure, Success, Try}

class GameConsoleInterpreter[F[_]: Sync]() extends GameAlgebra[F] {

  import scala.io.StdIn.readLine

  def clear: F[Unit] = {
    import scala.sys.process._
    Sync[F].delay("clear".!)
  }

  override def getWord: F[Word] =
    Sync[F].delay(println(s"Word to guess:")) >>
      Sync[F].defer(
        Try(readLine()).map(stringToNonEmptyList) match {
          case Failure(e) => clear >> Sync[F].delay(println(s"Try again -> ${e.toString}")) >> getWord
          case Success(None) => clear >> Sync[F].delay(println("Empty word")) >> getWord
          case Success(Some(w)) => clear >> Sync[F].delay(Word(w))
        }
      )

  override def getGuess: F[Guess] =
    Sync[F].delay(println(s"Guess letter:")) >>
      Sync[F].defer(
        Try(readLine()).map(_.toCharArray.toList) match {
          case Failure(e) => Sync[F].delay(println(s"Try again -> ${e.toString}")) >> getGuess
          case Success(x :: Nil) => Sync[F].delay(Guess(x))
          case Success(Nil) => Sync[F].delay(println("One letter")) >> getGuess
          case Success(_ :: _) => Sync[F].delay(println("One letter")) >> getGuess
        }
      )

  override def startNewGame: F[Boolean] =
    Sync[F].delay(println(s"New game? (Y/N):")) >>
      Sync[F].defer(
        Try(readLine()) match {
          case Failure(e) => clear >> Sync[F].delay(println(s"Try again -> ${e.toString}")) >> startNewGame
          case Success(x) if x.toLowerCase == "y" => clear >> Sync[F].delay(true)
          case Success(x) if x.toLowerCase == "n" => clear >> Sync[F].delay(false)
          case Success(_) => clear >> Sync[F].delay(println("Wrong answer")) >> startNewGame
        }
      )
}
