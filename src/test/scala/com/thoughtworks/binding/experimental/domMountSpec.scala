package com.thoughtworks.binding
package experimental

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, MultiMountPoint}
import com.thoughtworks.binding.experimental.dom.Runtime.TagsAndTags2
import com.thoughtworks.binding.experimental.dom.{Mount, toBindingSeq}
import org.scalajs.dom.html.HR
import org.scalatest.{FreeSpec, Inside, Matchers}

import scala.collection.GenSeq
import scala.scalajs.js

/**
  * @author 杨博 (Yang Bo)
  */
final class domMountSpec extends FreeSpec with Matchers with Inside {

  "Mount JavaScript WrappedArray" in {
    implicit def mountJsArray[Element, Children, Child](
        implicit toBindingSeqCase: toBindingSeq.Case.Aux[Children, BindingSeq[Child]],
        asSeq: Seq[Child] <:< Seq[Element]): Mount[js.WrappedArray[Element], Children] =
      new Mount[js.WrappedArray[Element], Children] {
        def mount(parent: js.WrappedArray[Element], children: Children): Binding[Unit] = {
          parent.toString()
          new MultiMountPoint[Child](toBindingSeqCase(children)) {

            protected def set(children: Seq[Child]): Unit = {
              parent.clear()
              parent ++= children
            }
            protected def splice(from: Int, that: GenSeq[Child], replaced: Int): Unit = {
              parent.splice(from, replaced, that.seq: _*)
            }
          }
        }
      }

    implicit def anyToBindingSeq[Element]: toBindingSeq.Case.Aux[js.WrappedArray[Element], BindingSeq[js.WrappedArray[Element]]] =
      toBindingSeq.at[js.WrappedArray[Element]](Constants(_))

    var arrayCount = 0
    implicit final class ArrayTagOps(tagsAndTags2: TagsAndTags2.type) {
      object array {
        def render = {
          arrayCount += 1
          js.WrappedArray.empty[Any]
        }
      }
    }

    @dom
    val a: Binding[js.WrappedArray[Any]] = <array><array/><hr/><array><array/><array/></array></array>

    a.watch()
    arrayCount should be(5)
    inside(a.get) {
      case js.WrappedArray(js.WrappedArray(), hr: HR, js.WrappedArray(js.WrappedArray(), js.WrappedArray())) =>
        hr.tagName.toLowerCase should be("hr")
    }

  }
}
