package hangman

import cats._
import cats.data._
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._

case class Word(value: NonEmptyList[Char]) {
  def contains(c: Char) = value exists (_ == c)
  def getLeftLetters(guesses: Set[Guess]) = (value filterNot (l => guesses.exists(_.value == l))).toSet
}
case class Guess(value: Char)

sealed trait BodyPart
case object Head extends BodyPart
case object Body extends BodyPart
case object LeftArm extends BodyPart
case object RightArm extends BodyPart
case object LeftLeg extends BodyPart
case object RightLeg extends BodyPart

object BodyPart {
  implicit def ordered: Order[BodyPart] = new Order[BodyPart] {

    private def toVal(bp: BodyPart): Int = bp match {
      case Head => 0
      case Body => 1
      case LeftArm => 2
      case RightArm => 3
      case LeftLeg => 4
      case RightLeg => 5
    }

    def compare(x: BodyPart, y: BodyPart): Int = toVal(x) - toVal(y)
  }
}

case class Game(
  word: Word,
  guesses: Set[Guess],
  state: GameState
) {
  def leftToGuess: Set[Char] = word getLeftLetters guesses
}

sealed trait GameState
case class InProgress(hanged: Set[BodyPart], left: NonEmptySet[BodyPart]) extends GameState
case object FinishedChallengerWon extends GameState
case class FinishedGuesserWon(hanged: Set[BodyPart]) extends GameState


object HangmanGame {

  def initialize(word: Word): Game =
    Game(word, Set.empty,
      InProgress(Set.empty, NonEmptySet.of(Head, Body, LeftArm, RightArm, LeftLeg, RightLeg)))

  def makeGuess(game: Game, guess: Guess): Game = {

    val (word, guesses, gameState) = (game.word, game.guesses, game.state)
    
    val lettersLeft = game.leftToGuess

    gameState match {
      case InProgress(hanged, left) => {
        (lettersLeft.size, lettersLeft contains guess.value, guesses contains guess, left.toNonEmptyList) match {
          case (_, _, true, _) | (0, _, _, _) => game
          case (_, false, _, NonEmptyList(h, Nil)) => Game(word, guesses + guess, FinishedChallengerWon)
          case (_, false, _, NonEmptyList(h, hh :: tail)) => Game(word, guesses + guess, InProgress(hanged + h, NonEmptySet.of(hh, tail:_*)))
          case (1, true, _, _) => Game(word, guesses + guess, FinishedGuesserWon(hanged))
          case (_, true, _, _) => Game(word, guesses + guess, gameState)
        }
      }
      case _ => game
    }
  }

  def guess[F[_]: FlatMap](guess: Guess)(implicit S: MonadState[F, Game]): F[Unit] =
    S.modify(gameState => makeGuess(gameState, guess))
}

object Main extends App {
  
  import HangmanGame._

  val word = Word(NonEmptyList('k', List('u', 'r', 'a', 'c')))

  def printt[F[_]: FlatMap](implicit S: MonadState[F, _]) =
    for {
      s <- S.get
      _ = println(s)
    } yield ()

  def program[F[_]: FlatMap](implicit S: MonadState[F, Game]) =
    for {
      _ <- guess[F](Guess('q'))
      _ <- printt[F]
      _ <- guess[F](Guess('r'))
      _ <- printt[F]
      _ <- guess[F](Guess('b'))
      _ <- printt[F]
      _ <- guess[F](Guess('a'))
      _ <- printt[F]
      _ <- guess[F](Guess('r'))
      _ <- printt[F]
      _ <- guess[F](Guess('r'))
      _ <- printt[F]
      _ <- guess[F](Guess('k'))
      _ <- printt[F]
      _ <- guess[F](Guess('u'))
      _ <- printt[F]
      _ <- guess[F](Guess('c'))
      _ <- printt[F]
    } yield ()

    type Program[T] = StateT[Id, Game, T]

    program[Program].run(initialize(word))

}
