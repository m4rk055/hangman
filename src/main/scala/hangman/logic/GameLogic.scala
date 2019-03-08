package hangman.logic

import cats.data.{NonEmptyList, NonEmptySet}
import hangman.domain.model._

object GameLogic {

  def initialize(game: Game, word: Word): Game =
    game.copy(gameState =
      Initialized(word, Set.empty, Set.empty,
        InProgress(NonEmptySet.of(Head, Body, LeftArm, RightArm, LeftLeg, RightLeg))))

  def makeGuess(game: Initialized, guess: Guess): Initialized = {

    val (word, guesses, gameState, hanged) = (game.word, game.guesses, game.state, game.hanged)

    val leftToGuess = word lettersToGuess guesses

    gameState match {
      case InProgress(left) =>
        (leftToGuess.size, leftToGuess contains guess.value, guesses contains guess, left.toNonEmptyList) match {
          case (_, _, true, _) | (0, _, _, _) => game

          case (_, false, _, NonEmptyList(h, Nil)) =>
            Initialized(word, guesses + guess, hanged + h, ChallengerWon)

          case (_, false, _, NonEmptyList(h, hh :: tail)) =>
            Initialized(word, guesses + guess, hanged + h, InProgress(NonEmptySet.of(hh, tail:_*)))

          case (1, true, _, _) =>
            Initialized(word, guesses + guess, hanged, GuesserWon)

          case (_, true, _, _) =>
            Initialized(word, guesses + guess, hanged, gameState)
        }
      case _ => game
    }
  }
}
