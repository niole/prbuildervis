package TimelineComponent

import DOMShortcuts.Shortcuts.DOM
import DotComponent.Dot
import PRBuilder.Data.{Build, Repo}

trait TimeLineDOM {
  def sToPx(ms: Int): Double
  def removeAll: Unit
  def render: List[DOM]
  def createDot(build: Build): Dot
}

class TimeLineRenderer(minTS: Double, maxTS: Double, repo: Repo, width: Double, height: Double, offsetTop: Int) extends DOM with TimeLineDOM {
  //render each dot on the timeline for this repo
  //minTS and maxTS are over all min and max timestamps for all visualized repos
  var renderedTimeLine = List[Dot]()
  val pxPerS = width/(maxTS-minTS)

  def sToPx(ms: Int): Double = ms*pxPerS

  def removeAll: Unit = {
    renderedTimeLine.foreach { (dot) =>
      dot.removeAll
    }
    renderedTimeLine = List[Dot]()
  }

  def render: List[DOM] = {
    renderedTimeLine = repo.builds.map { build =>
      val dot = createDot(build)
      modifyParent(dot.get)(append)
      dot
    }
    renderedTimeLine
  }

  def createDot(build: Build): Dot = {
    val success = build.details.success
    val background = success match {
      case true => "green"
      case false => "red"
    }

    val left = sToPx(build.details.created)
    val top = offsetTop * height + 7
    val inlineStyle = Map(
      "top" -> (top.toString + "px"),
      "left" -> (left.toString + "px"),
      "background" -> background,
      "height" -> "10px",
      "width" -> "10px",
      "border-radius" -> "5px",
      "display" -> "inline-block",
      "position" -> "absolute"
    )

    new Dot(inlineStyle, build)
  }
}