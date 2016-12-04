package DOMShortcuts

import org.scalajs.dom
import org.scalajs.dom._

object Shortcuts {
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

  class DOM {
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

  }

}
