package tutorial.webapp

import scala.scalajs.js
import org.scalajs.dom
import dom.document
import scala.scalajs.js.JSApp
import org.scalajs.dom.html

object TutorialApp extends JSApp {
  import PrBuilderVis._

  def main(): Unit = {
    appendPar(document.body, "hello world")
    val d = new DOM()
    val div = d.createElement("div", None, Some("cat"))
    d.modifyParent(div, None)(d.append)
    d.modifyParent(div, None)(d.remove)
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
}

object PrBuilderVis {
  import PrBuilderData._

  class DOM {
    //  example use:  val textArea = elementById[html.TextArea](textAreaID)
    type DOMModifier = (dom.raw.Element, Option[dom.raw.Element]) => Unit

    def elementById[A <: dom.Node](id: String): A =
      document.getElementById(id).asInstanceOf[A]

    def elementByClass[A <: dom.Node](className: String): A =
      document.getElementsByClassName(className).asInstanceOf[A]

    def createElement(elementTag: String, classNames: Option[List[String]], id: Option[String]): dom.raw.Element = {
      var element = document.createElement(elementTag)

      if (classNames.isDefined) {
        val className = classNames.mkString(" ")
        element.setAttribute("class", className)
      }

      if (id.isDefined) {
        id foreach {
          i => element.setAttribute("id", i)
        }
      }

      element
    }

    def modifyParent(child: dom.raw.Element, parent: Option[dom.raw.Element])(modifier: DOMModifier): Unit = {
      modifier(child, parent)
    }

    def append(child: dom.raw.Element, parent: Option[dom.raw.Element]): Unit = {
      if (parent.isDefined) {
        parent foreach { p => p.appendChild(child) }
      } else {
        document.body.appendChild(child)
      }
    }

    def remove(child: dom.raw.Element, parent: Option[dom.raw.Element]): Unit = {
      if (parent.isDefined) {
        parent foreach { p => p.removeChild(child) }
      } else {
        document.body.removeChild(child)
      }
    }

  }

  class TimeLineRenderer(minTS: Int, maxTS: Int, repo: Repo, width: Int, height: Int) extends DOM {
    //render each dot on the timeline for this repo
    //minTS and maxTS are over all min and max timestamps for all visualized repos
    //timestamps are in ms TODO maybe we can make that part of the time stamp type

    def render(repo: Repo): Unit = {
      repo.builds.foreach {
          case Build(id, name, link, created, buildDetails) =>
            id
      }
    }

  }
}

object PrBuilderData {
  type RepoName = String

  case class BuildDetails(details: Map[String, Any]) //TODO this is not a thing yet
  case class Build(id: String, name: String, link: String, created: Int, buildDetails: BuildDetails)
  case class Repos(repos: List[Repo])

  trait BaseRepo {
    type Builds = List[Build]
    def updateBuild(newBuild: Build): Unit
    var builds: Builds
  }

  class Repo extends BaseRepo {
    var builds = List[Build]()
    def updateBuild(newBuild: Build): Unit = {
      builds = builds.zipWithIndex.foldLeft(List[Build]())((builds, buildTuple) => {
        var nextBuilds = List[Build]()

        buildTuple match {
          case (b, index) =>
            if (b.id == newBuild.id) {
              nextBuilds = newBuild :: builds
            } else if (index == builds.length - 1) {
              //append to acc
              nextBuilds = b :: newBuild :: builds
            } else {
              nextBuilds = b :: builds
            }
        }

        nextBuilds
      })
    }
  }

}
