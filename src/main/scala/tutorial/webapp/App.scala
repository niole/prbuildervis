package App

import Actions.All.Action
import DOMShortcuts.Shortcuts.{DOM, NodeSpec}
import Store.Store.{Store, _}
import DOMShortcuts.Shortcuts.DOMNode
import PRBuilder.Data._
import PRBuilder.State._
import RepoComponent.Renderer.RepoRenderer

trait BaseApp {
  var input: DOMNode
  var repos: List[RepoRenderer]

  def createAll: Unit

  def createInput: DOMNode
  def createRepos(repos: List[Repo]): List[RepoRenderer]

  def mountInput: DOMNode
  def mountRepos: List[DOMNode]

  def removeInput: Unit
  def removeRepos: Unit
}

class App(store: Store, repoData: List[Repo]) extends DOM with BaseApp {

  var input = createElement(NodeSpec("span"))
  var repos = createRepos(repoData)


  def getInitialState: Unit = {
    createAll

    val allRepoNames = repoData.map { repo => repo.name }
    store.updateState[List[String], ActiveRepos](allRepoNames, ActiveRepos(allRepoNames))
  }

  //make all DOM elements
  def createAll: Unit = {
    createInput
    createRepos(repoData)
  }

  //put all DOM elements in DOM
  def mountAll: Unit = {
    mountInput
    mountRepos

  }

  //take all DOM elements out of DOM
  def removeAll: Unit = {
    removeInput
    removeRepos
  }

  def createInput: DOMNode = {
    input = createElement(NodeSpec("input", None, None, None, None, Some(Map("change" -> filterActiveRepos))))
    input
  }

  def filterActiveRepos(repoName: String): Unit = {
    //TODO update state with all repos that match this repoName
    val currState = store.get
    val nextActiveRepos = currState.activeRepos.names.filter { name => name == repoName}
    //TODO add in final argument which will rerender appropriate components
    store.updateState[List[String], ActiveRepos](nextActiveRepos, ActiveRepos(nextActiveRepos))
  }

  def createRepos(repos: List[Repo]): List[RepoRenderer] = {
    repos.zipWithIndex.map { case (repo, index) => {

      val max = 4000.0
      val min = 0.0
      val height = 100.0
      val repoRenderer = new RepoRenderer(repo, 800.0, height, min, max, index, store)
      repoRenderer
    }}
  }

  def mountInput: DOMNode = modifyParent(input)(append)

  def mountRepos: List[DOMNode] = repos.map { repo => repo.render }

  def removeInput: Unit = modifyParent(input, Some(input.parentNode))(remove)

  def removeRepos: Unit = repos.foreach{ repo => repo.remove }

}