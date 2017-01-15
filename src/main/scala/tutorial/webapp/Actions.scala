package Actions

import PRBuilder.State._

object All {
  abstract class Action[T] {
    def update[S](state: State, nextState: T, nextStateType: S): State
    def add[S](state: State, nextState: T, nextStateType: S): State
    def remove[S](state: State, nextState: T, nextStateType: S): State
  }

  implicit object ListStringAction extends Action[List[String]] {

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

  //updates relevant field in state with value
  //uses whole current state, next state value type, and next state value
  def update[T, S](state: State, nextState: T, nextStateType: S)(implicit action: Action[T]): State =
    action.update[S](state, nextState, nextStateType)

  def add[T, S](state: State, nextState: T, nextStateType: S)(implicit action: Action[T]): State =
    action.add[S](state, nextState, nextStateType)

  def remove[T, S](state: State, nextState: T, nextStateType: S)(implicit action: Action[T]): State =
    action.remove[S](state, nextState, nextStateType)

}