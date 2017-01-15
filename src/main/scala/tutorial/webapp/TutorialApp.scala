package tutorial.webapp

import scala.scalajs.js.JSApp

import PRBuilder.State._
import PRBuilder.Data.reposFixture
import Store.Store._
import App._

object TutorialApp extends JSApp {

  def main(): Unit = {

    val initialState = State(ActiveRepos(List[String]()))
    val store = new Store(initialState)

    val app = new App(store, reposFixture)
    app.getInitialState
    app.mountAll
  }
}
