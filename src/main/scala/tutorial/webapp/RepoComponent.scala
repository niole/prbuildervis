package RepoComponent

import TimelineComponent._
import DOMShortcuts.DOM._
import PRBuilder.Data.Repo

object Renderer {

  trait RepoDOM extends DOM {
    def render: DOMNode

    def remove: Unit
  }

  class RepoRenderer(repo: Repo, width: Double, height: Double, min: Double, max: Double, offset: Int) extends DOM with RepoDOM {
    var timeline = new TimeLineRenderer(min, max, repo, 800.0, height, offset)
    var renderedRepo = createElement(NodeSpec("span"))

    def render: DOMNode = {
      val top = (offset * height).toString
      val buttonStyle = Map(
        "position" -> "absolute",
        "background" -> "whitesmoke",
        "color" -> "darkbrown",
        "padding" -> "5px 15px",
        "border-radius" -> "2px",
        "left" -> "15px",
        "top" -> (top + "px")
      )

      val textNode = createText(repo.name)
      val repoButton = createElement(NodeSpec("div", Some(List(textNode)), None, None, Some(buttonStyle)))
      modifyParent(repoButton)(append)

      renderedRepo = repoButton
      timeline.render

      repoButton
    }

    def remove: Unit = {
      timeline.removeAll
      modifyParent(renderedRepo)(remove)
    }
  }

}