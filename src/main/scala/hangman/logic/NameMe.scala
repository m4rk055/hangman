package hangman.logic

import cats._
import cats.implicits._
import cats.mtl.MonadState
import hangman.domain.algebra._
import hangman.domain.model._

object NameMe {

  // just draw game
  def drawGameState[G[_]: FlatMap: PresentationAlgebra](game: Game): G[Unit] =
    game.gameState match {
      case Initializing => PresentationAlgebra[G].drawUninitializedGame(game)
      case Initialized(word, guesses, hanged, _) =>
        PresentationAlgebra[G].clear >>
          PresentationAlgebra[G].drawWord(word, guesses) >>
          PresentationAlgebra[G].drawGuesses(word, guesses) >>
          PresentationAlgebra[G].drawHangedMan(hanged)
    }

  def begin[F[_]: FlatMap, H[_]: FlatMap: GameAlgebra](implicit S: MonadState[F, Game], hToF: H ~> F): F[Unit] =
    hToF(GameAlgebra[H].getWord) >>=
      (word => S.modify(game => GameLogic.initialize(game, word)))

  def makeGuess[F[_]: FlatMap, G[_]: FlatMap: PresentationAlgebra, H[_]: GameAlgebra](
      implicit S: MonadState[F, Game], gToF: G ~> F, hToF: H ~> F): F[Unit] =
    for {
      game <- S.get
      _ <- gToF(drawGameState[G](game))
      _ <-
        game.gameState match {
          case Initializing => S.monad.pure(())
          case g@Initialized(_, _, _, state) =>
            state match {

              case ChallengerWon | GuesserWon =>
                S.monad.pure(())

              case InProgress(_) =>
                for {
                  guess <- hToF(GameAlgebra[H].getGuess)
                  _ <- S.set(game.copy(gameState = GameLogic.makeGuess(g, guess)))
                  _ <- makeGuess[F, G, H]
                } yield ()
            }
        }
    } yield ()

  def newGame[F[_]: FlatMap, G[_]: FlatMap: PresentationAlgebra, H[_]: FlatMap: GameAlgebra](
      implicit S: MonadState[F, Game], gToF: G ~> F, hToF: H ~> F): F[Unit] =
    hToF(GameAlgebra[H].startNewGame) >>=
      (
        if (_)
          begin[F, H] >>
          makeGuess[F, G, H] >>
          (S.get >>= (game => gToF(PresentationAlgebra[G].drawEndGame(game)))) >>
          newGame[F, G, H]
        else
          S.monad.pure(())
      )

  def play[F[_]: FlatMap, G[_]: FlatMap: PresentationAlgebra, H[_]: FlatMap: GameAlgebra](
      implicit S: MonadState[F, Game], gToF: G ~> F, hToF: H ~> F): F[Unit] =

    gToF(PresentationAlgebra[G].startApp) >>
      newGame[F, G, H]
}
