package hangman

import cats.data._
import cats.effect.{IO, Sync, _}
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import cats.{FlatMap, ~>}
import hangman.domain.algebra._
import hangman.domain.model._
import hangman.interpreters.console.{GameConsoleInterpreter, PresentationConsoleInterpreter}
import hangman.logic.NameMe

object Main extends IOApp {

  def runProgram[F[_]: Sync, G[_]: FlatMap: PresentationAlgebra, H[_]: FlatMap: GameAlgebra](
      implicit S: MonadState[F, Game], gToF: G ~> F, hToF: H ~> F): F[Unit] =
    NameMe.play[F, G, H]

  type Program[T] = StateT[IO, Game, T]

  implicit val IOGame: GameConsoleInterpreter[IO] = new GameConsoleInterpreter[IO]
  implicit val IOPres: PresentationConsoleInterpreter[IO] = new PresentationConsoleInterpreter[IO]

  implicit val ioToProgram: IO ~> Program = new ~>[IO, Program] {
    override def apply[A](fa: IO[A]): Program[A] = StateT(s => fa map ((s, _)))
  }

  val game = Game(Initializing)

  override def run(args: List[String]): IO[ExitCode] =
    runProgram[Program, IO, IO]
      .run(game)
      .attempt
      .map(_ leftMap (e => println(e.toString)))
      .as(ExitCode.Success)
}
