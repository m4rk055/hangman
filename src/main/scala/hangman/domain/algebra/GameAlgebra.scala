package hangman.domain.algebra

import hangman.domain.model.{Guess, Word}

trait GameAlgebra[F[_]] {
  def getWord: F[Word]
  def getGuess: F[Guess]
  def startNewGame: F[Boolean]
}

object GameAlgebra {
  def apply[F[_]](implicit g: GameAlgebra[F]): GameAlgebra[F] = g
}
