package DOMShortcuts

import org.scalajs.dom
import org.scalajs.dom._

object Shortcuts {
  type DOMNode = dom.raw.Node
  type DOMModifier = (DOMNode, Option[DOMNode]) => DOMNode

  type Tag = String
  type ClassName = Option[String]
  type Id = Option[String]
  type Style = Option[Map[String, String]]
  type Children = Option[List[DOMNode]]
  type DomHandlers = Option[Map[String, (Any) => Unit]]

  trait BaseNodeSpec {
    val tag: Tag
    val className: ClassName
    val id: Id
    val style: Style
    val children: Children
    val domHandlers: DomHandlers
  }

  case class NodeSpec(
                       override val tag: Tag,
                       override val children: Children = None,
                       override val className: ClassName = None,
                       override val id: Id = None,
                       override val style: Style = None,
                       override val domHandlers: DomHandlers = None
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