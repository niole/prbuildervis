package tutorial.webapp

import org.scalajs.dom
import dom.document
import scala.scalajs.js.JSApp

object TutorialApp extends JSApp {
  import PrBuilderData._
  import PrBuilderVis._

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

object PrBuilderVis {

  import PrBuilderData._
  import DOM._

  trait DotDOM extends DOM {
    def showTestData: Unit
    def hideTestData: Unit
    def removeAll: Unit
    def removePopup: Unit
    def removeDot: Unit
    def renderTestPopup: Unit
    def createTestPopup: DOMNode
    def setPopup(newPopup: DOMNode): Unit
    def get: DOMNode
  }

  class Dot(style: Map[String, String], private val build: Build) extends DOM with DotDOM {
    val dot = createElement(NodeSpec("div", None, Some("dot"), None, Some(style)))
    var popup = createTestPopup

    dot.addEventListener("mouseenter", { (e: dom.MouseEvent) =>
      showTestData
    })

    dot.addEventListener("mouseleave", { (e: dom.MouseEvent) =>
      hideTestData
    })

    def createTestPopup: DOMNode = {
      val testOutput = build.details.testOutput
      val textNodes = testOutput.map({ (text) =>
        createElement(NodeSpec("span", Some(List(createText(text))), Some("test-output")))
      })

      val popupStyle = Some(Map(
        "position" -> style("position"),
        "left" -> style("left"),
        "top" -> style("top")
      ))

      val p = createElement(NodeSpec("div", Some(textNodes), Some("test-popup"), None, popupStyle))
      setPopup(p)
      p
    }

    def renderTestPopup: Unit = {
      modifyParent(popup)(append)
    }

    def removePopup: Unit = {
      modifyParent(popup, Some(popup.parentNode))(remove)
    }

    def setPopup(newPopup: DOMNode): Unit = {
      popup = newPopup
    }

    def showTestData: Unit = {
      println("enter")
      renderTestPopup
    }

    def hideTestData: Unit = {
      println("leave")
      removePopup
    }


    def removeDot: Unit = {
      modifyParent(dot, Some(dot.parentNode))(remove)
    }

    def removeAll: Unit = {
      removeDot
      removePopup
    }

    def get: DOMNode = dot
  }

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

  trait RepoDOM extends DOM {
    def render: DOMNode
    def remove: Unit
  }

  class RepoRenderer(repo: Repo, width: Double, height: Double, min: Double, max: Double, offset: Int) extends DOM with RepoDOM {
    var timeline = new TimeLineRenderer(min, max, repo, 800.0, height, offset)
    var renderedRepo = createElement(NodeSpec("span"))

    def render: DOMNode = {
      val top = (offset*height).toString
      val buttonStyle = Map(
        "position" -> "absolute",
        "background" -> "whitesmoke",
        "color" -> "darkbrown",
        "padding" -> "5px 15px",
        "border-radius" -> "2px",
        "left" -> "15px",
        "top" -> (top+"px")
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

object PrBuilderData {
  var reposFixture = List(new Repo("repo1"), new Repo("hello world"), new Repo("mySexyRepo"))
  val buildDetails = List(
    BuildDetails(true, 1000, List("this is a test, it suceeded")),
    BuildDetails(false, 2000, List("this is a test, it failed")),
    BuildDetails(true, 3000, List("this is a test, it failed", "this is a test, it failed"))
  )

  val buildsFixture = buildDetails.map(bd => Build(math.random().toString(), "build_name", "https://google.com", bd))
  reposFixture = reposFixture.map(r => {
    buildsFixture.foreach(build => {
      r.addBuild(build)
    })
    r
  })


  type RepoName = String

  case class BuildDetails(success: Boolean, created: Int, testOutput: List[String])
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
    type DOMNode = dom.raw.Node
    type DOMModifier = (DOMNode, Option[DOMNode]) => DOMNode

    trait BaseNodeSpec {
      val tag: String
      val className: Option[String]
      val id: Option[String]
      val style: Option[Map[String, String]]
      val children: Option[List[DOMNode]]
    }

    case class NodeSpec(
                        override val tag: String,
                        override val children: Option[List[DOMNode]] = None,
                        override val className: Option[String] = None,
                        override val id: Option[String] = None,
                        override val style: Option[Map[String, String]] = None
                       ) extends BaseNodeSpec

    def elementById[A <: dom.Node](id: String): A =
      document.getElementById(id).asInstanceOf[A]

    def elementByClass[A <: dom.Node](className: String): A =
      document.getElementsByClassName(className).asInstanceOf[A]

    def createElement(nodeSpec: NodeSpec): DOMNode = {
      val parent = document.createElement(nodeSpec.tag)

      nodeSpec.className.foreach { classes =>
          parent.setAttribute("class", classes)
      }

      nodeSpec.id.foreach { i => parent.setAttribute("id", i) }

      nodeSpec.style.foreach { s =>
        val totalStyle = s.foldLeft("") { case (styleString, (k, v)) => styleString + s"$k: $v;" }
        parent.setAttribute("style", totalStyle)
      }

      nodeSpec.children.foreach { childs =>
        childs.foreach { nextChild =>
          modifyParent(nextChild, Some(parent))(append)
        }
      }

      parent
    }

    def createText(text: String): DOMNode = document.createTextNode(text)

    def modifyParent(child: DOMNode, parent: Option[DOMNode] = Some(document.body))(modifier: DOMModifier): DOMNode = {
      modifier(child, parent)
    }

    def append(child: DOMNode, parent: Option[DOMNode] = Some(document.body)): DOMNode = {
      parent foreach { p => p.appendChild(child) }
      child
    }

    def remove(child: DOMNode, parent: Option[DOMNode] = Some(document.body)): DOMNode = {
      parent foreach { p => p.removeChild(child) }
      child
    }

    ///def show(node: DOMNode): Unit = {
    ///  node.setAttribute("style", "visibility: visible;")
    ///}

    ///def hide(node: DOMNode): Unit = {
    ///  node.setAttribute("style", "visibility: hidden;")
    ///}

  }
}
