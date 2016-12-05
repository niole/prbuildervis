package Actions

import PRBuilder.State._

abstract class SemiGroup[A] {
  def add(x: A, y: A): A
}
abstract class Monoid[A] extends SemiGroup[A] {
  def unit: A
}

object ImplicitTest extends App {

  implicit object StringMonoid extends Monoid[String] {
    def add(x: String, y: String): String = x concat y
    def unit: String = ""
  }
  implicit object IntMonoid extends Monoid[Int] {
    def add(x: Int, y: Int): Int = x + y
    def unit: Int = 0
  }

  def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
  if (xs.isEmpty) m.unit
  else m.add(xs.head, sum(xs.tail))

  println(sum(List(1, 2, 3)))          // uses IntMonoid implicitly
  println(sum(List("a", "b", "c")))    // uses StringMonoid implicitly
}


object All {
  abstract class Action[T] {
    def update[S](state: State, nextState: T, nextStateType: S): State
    def add[S](state: State, nextState: T, nextStateType: S): State
    def remove[S](state: State, nextState: T, nextStateType: S): State
  }

  implicit object X extends Action[List[String]] {

    def update[S](state: State, nextState: List[String], nextStateType: S): State = nextStateType match {
      case ActiveRepos(_) =>
        state.copy(activeRepos = ActiveRepos(nextState))
    }

    def add[S](state: State, nextState: List[String], nextStateType: S): State = state match {
      case State(activeRepos) => nextStateType match {
        case ActiveRepos(names) =>
          state.copy(activeRepos = ActiveRepos(nextState ++ names))
      }
    }

    def remove[S](state: State, nextState: List[String], nextStateType: S): State = state match {
      case State(oldActiveRepos) => nextStateType match {
        case ActiveRepos(namesToDelete) =>
          val newNames = oldActiveRepos.names.foldRight(List[String]()) { (name, acc) =>
            val indexToDelete = namesToDelete.indexOf(name)
            if (indexToDelete > -1) acc
            else name :: acc
          }
          state.copy(activeRepos = ActiveRepos(newNames))
      }
    }
  }

  def update[T, S](state: State, nextState: T, nextStateType: S)(implicit action: Action[T]): State =
    action.update[S](state, nextState, nextStateType)

  def add[T, S](state: State, nextState: T, nextStateType: S)(implicit action: Action[T]): State =
    action.add[S](state, nextState, nextStateType)

  def remove[T, S](state: State, nextState: T, nextStateType: S)(implicit action: Action[T]): State =
    action.remove[S](state, nextState, nextStateType)

}