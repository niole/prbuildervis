package tutorial.webapp

import scala.scalajs.js
import org.scalajs.dom
import dom.document
import scala.scalajs.js.JSApp

object TutorialApp extends JSApp {
  import PrBuilderData._
  import PrBuilderVis._

  def main(): Unit = {
    val (renderedRepos: List[(RepoRenderer, TimeLineRenderer)]) = reposFixture.zipWithIndex.map{case (repo, index) => {
      val max = 4000.0
      val min = 0.0
      val height = 100.0
      val timeline = new TimeLineRenderer(min, max, repo, 800.0, height, index)
      val repoRenderer = new RepoRenderer(repo, height, index)
      repoRenderer.render
      timeline.render
      (repoRenderer, timeline)
    }}
  }
}

object PrBuilderVis {
  import PrBuilderData._
  import DOM._

  class RepoRenderer(repo: Repo, height: Double, offset: Int) extends DOM {
    def render: ChildDOMNode = {
      val top = (offset*height).toString
      val buttonStyle = Map(
        "position" -> "absolute",
        "background" -> "whitesmoke",
        "color" -> "darkbrown",
        "height" -> "75px",
        "width" -> "150px",
        "left" -> "5px",
        "top" -> (top+"px")
      )
      val repoButton = createElement("div", None, None, Some(buttonStyle))
      val textNode = document.createTextNode(repo.name)
      modifyParent(textNode, Some(repoButton))(append)
      modifyParent(repoButton)(append)
    }
  }


  class TimeLineRenderer(minTS: Double, maxTS: Double, repo: Repo, width: Double, height: Double, offsetTop: Int) extends DOM {
    //render each dot on the timeline for this repo
    //minTS and maxTS are over all min and max timestamps for all visualized repos
    val pxPerS = width/(maxTS-minTS)

    def sToPx(ms: Int): Double = ms*pxPerS

    def render: List[ChildDOMNode] = repo.builds.map { build =>
      modifyParent(createDot(build))(append)
    }

    def createDot(build: Build, d: DOM = this): ChildDOMNode = {
      val success = build.details.success
      val background = success match {
        case true => "green"
        case false => "red"
      }

      val left = sToPx(build.details.created)
      val top = offsetTop * height
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
      createElement("div", Some(List("dot")), None, Some(inlineStyle))
    }
  }
}

object PrBuilderData {
  var reposFixture = List(new Repo("repo1"), new Repo("hello world"), new Repo("mySexyRepo"))
  val buildDetails = List(
    BuildDetails(true, 1000),
    BuildDetails(false, 2000),
    BuildDetails(true, 3000)
  )

  val buildsFixture = buildDetails.map(bd => Build(math.random().toString(), "build_name", "https://google.com", bd))
  reposFixture = reposFixture.map(r => {
    buildsFixture.foreach(build => {
      r.addBuild(build)
    })
    r
  })


  type RepoName = String

  case class BuildDetails(success: Boolean, created: Int)
  case class Build(id: String, name: String, link: String, details: BuildDetails)
  case class Repos(repos: List[Repo])

  trait BaseRepo {
    var builds: Builds
    type Builds = List[Build]
    def updateBuilds(newBuild: Build): Unit
    def addBuild(newBuild: Build): Unit
  }

  class Repo(repoName: String) extends BaseRepo {
    val name = repoName
    var builds = List[Build]()
    def updateBuilds(newBuild: Build): Unit = {
      builds = builds.map(build => {
        if (build.id == newBuild.id) newBuild
        else build
      })
    }

    def addBuild(newBuild: Build): Unit = {
      builds = newBuild :: builds
    }
  }

}

object DOM {

  class DOM {
    type ChildDOMNode = dom.raw.Node
    type DOMModifier = (ChildDOMNode, Option[dom.raw.Element]) => ChildDOMNode

    def elementById[A <: dom.Node](id: String): A =
      document.getElementById(id).asInstanceOf[A]

    def elementByClass[A <: dom.Node](className: String): A =
      document.getElementsByClassName(className).asInstanceOf[A]

    def createElement(elementTag: String, classNames: Option[List[String]], id: Option[String], style: Option[Map[String, String]]): dom.raw.Element = {
      var element = document.createElement(elementTag)

      classNames foreach { cns =>
          val classes = cns.mkString(" ")
          element.setAttribute("class", classes)
      }

      id foreach {
        i => element.setAttribute("id", i)
      }

      style foreach { s =>
        val totalStyle = s.foldLeft("") { case (styleString, (k, v)) => styleString + s"$k: $v;" }
        element.setAttribute("style", totalStyle)
      }

      element
    }

    def modifyParent(child: ChildDOMNode, parent: Option[dom.raw.Element] = Some(document.body))(modifier: DOMModifier): ChildDOMNode = {
      modifier(child, parent)
    }

    def append(child: ChildDOMNode, parent: Option[dom.raw.Element] = Some(document.body)): ChildDOMNode = {
      parent foreach { p => p.appendChild(child) }
      child
    }

    def remove(child: ChildDOMNode, parent: Option[dom.raw.Element] = Some(document.body)): ChildDOMNode = {
      parent foreach { p => p.removeChild(child) }
      child
    }

  }

}
