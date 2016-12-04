package tutorial.webapp

import PRBuilder.Data._
import RepoComponent.Renderer._
import scala.scalajs.js.JSApp

object TutorialApp extends JSApp {

  def main(): Unit = {
    val (renderedRepos: List[RepoRenderer]) = reposFixture.zipWithIndex.map{case (repo, index) => {
      val max = 4000.0
      val min = 0.0
      val height = 100.0
      val repoRenderer = new RepoRenderer(repo, 800.0, height, min, max, index)
      repoRenderer.render
      repoRenderer
    }}
  }
}
