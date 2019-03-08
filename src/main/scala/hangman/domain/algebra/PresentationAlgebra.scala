package hangman.domain.algebra

import hangman.domain.model._

trait PresentationAlgebra[F[_]] {

  def startApp: F[Unit]

  def drawUninitializedGame(game: Game): F[Unit]

  def drawWord(word: Word, guesses: Set[Guess]): F[Unit]

  def drawGuesses(word: Word, guesses: Set[Guess]): F[Unit]

  def drawHangedMan(hanged: Set[BodyPart]): F[Unit]

  def drawEndGame(game: Game): F[Unit]

  def clear: F[Unit]
}

object PresentationAlgebra {
  def apply[F[_]](implicit p: PresentationAlgebra[F]): PresentationAlgebra[F] = p
}
