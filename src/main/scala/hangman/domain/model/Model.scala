package hangman.domain.model

import cats.Order
import cats.data.{NonEmptyList, NonEmptySet}

case class Word(value: NonEmptyList[Char]) {
  def contains(c: Char): Boolean = value exists (_ == c)
  def lettersToGuess(guesses: Set[Guess]): Set[Char] =
    (value filterNot (l => guesses.exists(_.value == l))).toSet
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
    gameState: GameState,
    info: Option[String] = None,
)

sealed trait GameState

case object Initializing extends GameState

case class Initialized(
    word: Word,
    guesses: Set[Guess],
    hanged: Set[BodyPart],
    state: GameProgressState,
) extends GameState


sealed trait GameProgressState
case class InProgress(left: NonEmptySet[BodyPart]) extends GameProgressState

sealed trait GameFinished extends GameProgressState
case object ChallengerWon extends GameFinished
case object GuesserWon extends GameFinished
