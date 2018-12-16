/*
The MIT License (MIT)

Copyright (c) 2016 Yang Bo & REA Group Ltd.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

package com.thoughtworks.binding
package experimental

import Binding.{BindingSeq, Constants, MultiMountPoint, SingleMountPoint, SingletonBindingSeq}
import dom.Runtime.{NodeMountPoint, NodeSeqMountPoint}
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor.{PrefixedName, QName, UnprefixedName}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle
import org.scalajs.dom.raw._

import scala.annotation.{StaticAnnotation, compileTimeOnly, implicitNotFound, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scalatags.JsDom
import scalatags.jsdom
import org.scalajs.dom.document
import scalatags.JsDom.TypedTag
import scalatags.generic.Namespace
import shapeless.Poly1
import shapeless.ops.tuple.{Mapper, ToTraversable}

import scala.collection.immutable.Queue
import scala.language.higherKinds
import scala.reflect.NameTransformer

/**
  * Enable XML DOM literal for Binding.scala
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
// TODO: @dom will be deprecated once @html is implemented
// @deprecated(message = "Use `@html` instead", since = "11.0.0")
class dom extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro dom.Macros.macroTransform
}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
// TODO: @dom will be deprecated once @html is implemented
// @deprecated(message = "Use `@html` instead", since = "11.0.0")
object dom {

  private[dom] trait LowPriorityToBindingSeq0 extends Poly1 {

    implicit def tupleToBindingSeq[Tuple <: Product with Serializable, MappedTuple, Children, Child](
        implicit
        mapper: Mapper.Aux[Tuple, toBindingSeq.type, MappedTuple],
        toArray: ToTraversable.Aux[MappedTuple, Array, Children],
        asBindingSeq: Children <:< BindingSeq[Child]): Case.Aux[Tuple, BindingSeq[Child]] = at[Tuple] { tuple =>
      new BindingSeq.FlatMap(Constants(toArray(mapper(tuple)): _*), asBindingSeq)
    }

    implicit def nodeToBindingSeq[Child <: Node]: Case.Aux[Child, BindingSeq[Child]] =
      at[Child](Constants(_))

    implicit def bindingToBindingSeq[BindingSubtype[x] <: Binding[x], Children, Child](
        implicit childrenToBindingSeq: Case.Aux[Children, BindingSeq[Child]]
    ): Case.Aux[BindingSubtype[Children], BindingSeq[Child]] = at[BindingSubtype[Children]] { bindingChildren =>
      new BindingSeq.FlatMap(SingletonBindingSeq(bindingChildren), { x: Children =>
        childrenToBindingSeq(x)
      })
    }

    implicit def bindingSeqToBindingSeq[BindingSeqSubtype[x] <: BindingSeq[x], Child]
      : Case.Aux[BindingSeq[Child], BindingSeq[Child]] = {
      at[BindingSeq[Child]](identity)
    }

    implicit def scalaSeqToBindingSeq[ImmutableSeq[x] <: collection.immutable.Seq[x], Child]
      : Case.Aux[ImmutableSeq[Child], BindingSeq[Child]] = {
      at[ImmutableSeq[Child]](Constants(_: _*))
    }

    implicit def arrayToBindingSeq[Child]: Case.Aux[Array[Child], BindingSeq[Child]] = {
      at[Array[Child]](Constants(_: _*))
    }

    implicit def optionToBindingSeq[OptionSubtype[x] <: Option[x], Child]
      : Case.Aux[OptionSubtype[Child], BindingSeq[Child]] = {
      at[OptionSubtype[Child]] { o =>
        Constants(o.toSeq: _*)
      }
    }

    implicit def stringToBindingSeq: Case.Aux[String, BindingSeq[Text]] =
      at[String] { s =>
        Constants(document.createTextNode(s))
      }

  }

  /** An ad-hoc polymorphic function that converts things to [[BindingSeq]]. */
  object toBindingSeq extends LowPriorityToBindingSeq0 {

    /** Returns an optimized [[Case]] that behaves the same as `bindingToBindingSeq(bindingSeqToBindingSeq)` */
    implicit def bindingBindingSeqToBindingSeq[Child]: Case.Aux[Binding[BindingSeq[Child]], BindingSeq[Child]] =
      at[Binding[BindingSeq[Child]]] { bindingBindingSeq =>
        new BindingSeq.FlatMap(SingletonBindingSeq(bindingBindingSeq), identity[BindingSeq[Child]])
      }

    /** Returns an optimized [[Case]] that behaves the same as `bindingToBindingSeq(nodeToBindingSeq)` */
    implicit def bindingNodeToBindingSeq[Child <: Node]: Case.Aux[Binding[Child], BindingSeq[Child]] =
      at[Binding[Child]](SingletonBindingSeq(_))

    /** Returns an optimized [[Case]] that behaves the same as `bindingToBindingSeq(stringToBindingSeq)` */
    implicit def bindingStringToBindingSeq: Case.Aux[Binding[String], BindingSeq[Text]] =
      at[Binding[String]] { bindingString =>
        SingletonBindingSeq(Binding(document.createTextNode(bindingString.bind)))
      }

  }

  private[dom] sealed trait LowPriorityMount0 {
    implicit final def mountNodeSeq[Parent <: Node, Component, NodeSeq](
        implicit renderCase: toBindingSeq.Case.Aux[Component, NodeSeq],
        asNodeSeq: NodeSeq <:< BindingSeq[Node]
    ): Mount[Parent, Component] = {
      new Mount[Parent, Component] {
        def mount(parent: Parent, component: Component): Binding[Unit] = {
          new NodeSeqMountPoint(parent, asNodeSeq(renderCase(component)))
        }
      }
    }
  }

  object Mount extends LowPriorityMount0 {
    implicit final def mountBindingSeqNode[Parent <: Node, Child <: Node]: Mount[Parent, BindingSeq[Child]] =
      new Mount[Parent, BindingSeq[Child]] {
        def mount(parent: Parent, children: BindingSeq[Child]): NodeSeqMountPoint = {
          new NodeSeqMountPoint(parent, children)
        }
      }

    implicit final def mountBindingNode[Parent <: Node, Child <: Node]: Mount[Parent, Binding[Child]] =
      new Mount[Parent, Binding[Child]] {
        def mount(parent: Parent, child: Binding[Child]): NodeMountPoint = {
          new NodeMountPoint(parent, child)
        }
      }
  }

  @implicitNotFound("Don't know how to mount ${Children} into ${Parent}.")
  trait Mount[-Parent, -Children] {
    def mount(parent: Parent, children: Children): Binding[Unit]
  }

  private[dom] sealed trait LowPriorityRuntime {
    @inline
    final def notEqual[A, B](left: A, right: B, dummy: Unit = ()): Boolean = left != right
  }

  @inline
  @tailrec
  private def removeAll(parent: Node): Unit = {
    val firstChild = parent.firstChild
    if (firstChild != null) {
      parent.removeChild(firstChild)
      removeAll(parent)
    }
  }

  /**
    * Internal helpers for `@dom` annotation
    *
    * @note Do not use methods and classes in this object.
    */
  object Runtime extends LowPriorityRuntime {

    final class NodeMountPoint private[dom] (parent: Node, childBinding: Binding[Node])
        extends SingleMountPoint[Node](childBinding) {
      protected def set(child: Node): Unit = {
        removeAll(parent)
        checkedAppendChild(parent, child)
      }
    }

    final class NodeSeqMountPoint private[dom] (parent: Node, childrenBinding: BindingSeq[Node])
        extends MultiMountPoint[Node](childrenBinding) {

      override protected def set(children: Seq[Node]): Unit = {
        removeAll(parent)
        for (child <- children) {
          checkedAppendChild(parent, child)
        }
      }

      override protected def splice(from: Int, that: GenSeq[Node], replaced: Int): Unit = {
        @inline
        @tailrec
        def removeChildren(child: Node, n: Int): Node = {
          if (n == 0) {
            child
          } else {
            val nextSibling = child.nextSibling
            parent.removeChild(child)
            removeChildren(nextSibling, n - 1)
          }
        }

        val child = removeChildren(parent.childNodes(from), replaced)
        if (child == null) {
          for (newChild <- that) {
            checkedAppendChild(parent, newChild)
          }
        } else {
          for (newChild <- that) {
            if (newChild.parentNode != null) {
              throw new IllegalStateException(raw"""Cannot insert a ${newChild.nodeName} element twice!""")
            }
            parent.insertBefore(newChild, child)
          }
        }
      }

    }

    object TagsAndTags2 extends JsDom.Cap with jsdom.Tags with jsdom.Tags2 {

      import scala.language.dynamics

      final class DynamicDataTag private[TagsAndTags2] ()
          extends TypedTag[HTMLElement]("data", Nil, false, Namespace.htmlNamespaceConfig)
          with Dynamic {
        final def selectDynamic(tagName: String): ConcreteHtmlTag[Element] = {
          TagsAndTags2.tag(tagName)
        }
      }

      override lazy val data = new DynamicDataTag()

    }

    @inline
    def notEqual[A](left: A, right: A) = left != right
  }

  private def checkedAppendChild(parent: Node, child: Node): Unit = {
    if (child.parentNode != null) {
      throw new IllegalStateException(raw"""Cannot insert ${child.nodeName} twice!""")
    }
    parent.appendChild(child)
  }

  /**
    * This object contains implicit views imported automatically for @dom methods.
    */
  object AutoImports {

    implicit final class DataOps @inline()(node: Element) {

      import scala.language.dynamics

      object data extends Dynamic {

        final def selectDynamic(attributeName: String): String = {
          node.getAttribute(attributeName)
        }

        final def updateDynamic(attributeName: String)(attributeValue: String): Unit = {
          node.setAttribute(attributeName, attributeValue)
        }

      }

    }

    implicit final class OptionOps @inline()(node: Element) {

      import scala.language.dynamics

      object option extends Dynamic {

        final def selectDynamic(attributeName: String): Option[String] = {
          if (node.hasAttribute(attributeName)) {
            Some(node.getAttribute(attributeName))
          } else {
            None
          }
        }

        final def updateDynamic(attributeName: String)(attributeValue: Option[String]): Unit = {
          attributeValue.fold(node.removeAttribute(attributeName))(node.setAttribute(attributeName, _))
        }

      }

    }

    final class StyleOps @inline @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")(
        node: HTMLElement) {
      @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")
      @inline def style = node.style.cssText

      @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")
      @inline def style_=(value: String) = node.style.cssText = value
    }

    @deprecated("Use [[org.scalajs.dom.raw.HTMLElement.style]] instead", "11.2.0")
    def StyleOps(node: HTMLElement) = new StyleOps(node)

    implicit final class ClassOps @inline()(node: HTMLElement) {
      @inline def `class` = node.className

      @inline def class_=(value: String) = node.className = value
    }

    implicit final class ForOps @inline()(node: HTMLLabelElement) {
      @inline def `for` = node.htmlFor

      @inline def for_=(value: String) = node.htmlFor = value
    }

    @inline def workaroundUnusedImport() = ()
  }

  @inline
  def mount[Parent, Children](parent: Parent, children: Children)(
      implicit mount: Mount[Parent, Children]): Binding[Unit] = {
    mount.mount(parent, children)
  }

  /**
    * Render `children` into `parent`
    */
  @inline
  def render[Parent, Children](parent: Parent, children: Children)(implicit mount: Mount[Parent, Children]): Unit = {
    mount.mount(parent, children).watch()
  }

  @bundle
  private[dom] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {

        private def transformXml(tree: Tree): (Queue[ValDef], Tree) = {
          tree match {
            case transformedWithValDefs.extract(queue, tree) =>
              (queue, tree)
            case transformed.extract(transformedTree) =>
              Queue.empty -> transformedTree
            case _ =>
              Queue.empty -> super.transform(tree)
          }
        }

        private def nodeSeq(children: Seq[Tree]): (Queue[ValDef], Tree) = {
          val transformedPairs = (for {
            child <- children
          } yield {
            val (valDefs, transformedChild) = transformXml(child)
            valDefs -> q"_root_.com.thoughtworks.binding.Binding.apply($transformedChild)"
          })(collection.breakOut(Queue.canBuildFrom))
          val (valDefs, transformedChildren) = transformedPairs.unzip
          valDefs.flatten -> q"""(..$transformedChildren)"""
        }

        private def transformedWithValDefs: PartialFunction[Tree, (Queue[ValDef], Tree)] = {
          case tree @ NodeBuffer(children) =>
            nodeSeq(children)
          case tree @ Elem(tag, attributes, _, children) =>
            val idOption = findTextAttribute("local-id", attributes).orElse(findTextAttribute("id", attributes))
            val elementName = idOption match {
              case None     => TermName(c.freshName("htmlElement"))
              case Some(id) => TermName(NameTransformer.encode(id))
            }

            val attributeMountPoints = for {
              (key, value) <- attributes if {
                key match {
                  case UnprefixedName("local-id") => false
                  case _                          => true
                }
              }
            } yield {
              val attributeAccess = propertyAccess(key, q"$elementName")

              atPos(value.pos) {
                value match {
                  case EmptyAttribute() =>
                    q"""$attributeAccess = "" """
                  case Text(textLiteral) =>
                    q"$attributeAccess = $textLiteral"
                  case _ =>
                    val assignName = TermName(c.freshName("assignAttribute"))
                    val newValueName = TermName(c.freshName("newValue"))
                    q"""
                      _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                        _root_.com.thoughtworks.binding.Binding,
                        _root_.scala.Unit
                      ](
                        _root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Unit]({
                          val $newValueName = ${transform(value)}
                          @_root_.scala.inline def $assignName() = {
                            if (_root_.com.thoughtworks.binding.experimental.dom.Runtime.notEqual($attributeAccess, $newValueName)) {
                              $attributeAccess = $newValueName
                            }
                          }
                          $assignName()
                        })
                      )
                    """
                }
              }
            }
            val (valDefs, transformedChild) = children match {
              case Seq() =>
                Queue.empty -> Nil
              case _ =>
                val (valDefs, transformedBuffer) = nodeSeq(children)
                valDefs -> List(atPos(tree.pos) {
                  q"""
                  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                    _root_.com.thoughtworks.binding.Binding,
                    _root_.scala.Unit
                  ](
                    _root_.com.thoughtworks.binding.experimental.dom.mount(
                      $elementName,
                      $transformedBuffer
                    )
                  )
                  """
                })
            }

            val tagAccess =
              propertyAccess(tag, q"_root_.com.thoughtworks.binding.experimental.dom.Runtime.TagsAndTags2")

            val elementDef = q"val $elementName = $tagAccess.render"
            idOption match {
              case None =>
                valDefs -> q"""
                  $elementDef
                  ..$transformedChild
                  ..$attributeMountPoints
                  $elementName
                """
              case Some(id) =>
                (valDefs.enqueue(elementDef)) -> q"""
                  ..$transformedChild
                  ..$attributeMountPoints
                  $elementName
                """
            }
        }

        private def findTextAttribute(unprefixedName: String,
                                      attributes: Seq[(XmlExtractor.QName, Tree)]): Option[String] = {
          attributes.collectFirst { case (UnprefixedName(`unprefixedName`), Text(text)) => text }
        }

        private def propertyAccess(xmlName: QName, objectAccess: RefTree): Select = {
          xmlName match {
            case UnprefixedName(localPart) =>
              q"$objectAccess.${TermName(NameTransformer.encode(localPart))}"
            case PrefixedName(prefix, localPart) =>
              localPart.split(':').foldLeft(q"$objectAccess.${TermName(NameTransformer.encode(prefix))}") {
                (prefixExpr, segmentName) =>
                  q"$prefixExpr.${TermName(NameTransformer.encode(segmentName))}"
              }
          }
        }

        private def transformed: PartialFunction[Tree, Tree] = {
          case Block(stats, expr) =>
            super.transform(Block(stats.flatMap {
              case transformedWithValDefs.extract((valDefs, transformedTree)) =>
                valDefs.enqueue(transformedTree)
              case stat =>
                Seq(stat)
            }, expr))
          case tree @ EntityRef(HtmlEntityName(unescapedCharacter)) =>
            atPos(tree.pos) {
              q"""$unescapedCharacter"""
            }
          case tree @ Comment(value) =>
            atPos(tree.pos) {
              q"""_root_.org.scalajs.dom.document.createComment($value)"""
            }
          case tree @ Text(value) =>
            atPos(tree.pos) {
              q"$value"
            }
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case transformedWithValDefs.extract((valDefs, transformedTree)) =>
              q"""
                ..$valDefs
                $transformedTree
              """
            case transformed.extract(transformedTree) =>
              transformedTree
            case _ =>
              super.transform(tree)
          }
        }
      }

      import transformer.transform
      //      def transform(tree: Tree): Tree = {
      //        val output = transformer.transform(tree)
      //        c.info(c.enclosingPosition, show(output), true)
      //        output
      //      }

      def autoImportAndTransform(body: Tree) = {
        q"""_root_.com.thoughtworks.binding.Binding.apply {
          import _root_.com.thoughtworks.binding.experimental.dom.AutoImports.{
            != => _,
            ## => _,
            == => _,
            eq => _,
            equals => _,
            getClass => _,
            hashCode => _,
            ne => _,
            notify => _,
            notifyAll => _,
            synchronized => _,
            toString => _,
            wait => _,
            _
          }
          workaroundUnusedImport()
          ${transform(body)}
        }"""
      }
      replaceDefBody(annottees, autoImportAndTransform)
    }

  }

}
