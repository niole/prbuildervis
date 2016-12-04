package DotComponent

import PRBuilder.Data._
import DOMShortcuts.DOM.{DOM, NodeSpec, _}
import org.scalajs.dom

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