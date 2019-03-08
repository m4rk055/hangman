package hangman

import cats.data.NonEmptyList
import cats.implicits._

object Util {

  def stringToNonEmptyList(s: String): Option[NonEmptyList[Char]] = {
    s.toCharArray.toList match {
      case h :: tail => NonEmptyList(h, tail).some
      case Nil => none
    }
  }
}
