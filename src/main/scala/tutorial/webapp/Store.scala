package Store

import Actions.All.{update, add, remove, Action}
import PRBuilder.State._
import DOMShortcuts.Shortcuts.DOMNode

object Store {

  type DOMUpdater = Option[(State) => DOMNode]

  trait BaseStore {
    var states: List[State]
    def updateState[T: Action, S](nextValue: T, nextValueType: S, handler: DOMUpdater): State
    def addState[T: Action, S](nextValue: T, nextValueType: S, handler: DOMUpdater): State
    def removeState[T: Action, S](nextValue: T, nextValueType: S, handler: DOMUpdater): State
    def get: State
  }

  class Store(initialState: State) extends BaseStore {
    override var states = List(initialState)

    private def addToStateCollection(nextState: State, optionalHandler: DOMUpdater): State = {
      states = nextState :: states

      optionalHandler.foreach{ handler => handler(nextState)}

      nextState
    }

    //updates the state with any value type and respective action
    //triggers DOM updating handler on state update

    def updateState[T: Action, S](nextValue: T, nextValueType: S, handler: DOMUpdater = None): State =
      addToStateCollection(update[T, S](get, nextValue, nextValueType), handler)

    def addState[T: Action, S](nextValue: T, nextValueType: S, handler: DOMUpdater = None): State =
      addToStateCollection(add[T, S](get, nextValue, nextValueType), handler)

    def removeState[T: Action, S](nextValue: T, nextValueType: S, handler: DOMUpdater = None): State =
      addToStateCollection(remove[T, S](get, nextValue, nextValueType), handler)

    def get: State = states.head
  }
}
