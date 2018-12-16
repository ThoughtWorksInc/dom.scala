package com.thoughtworks.binding
package experimental

import com.thoughtworks.binding.experimental.dom.Mount
import org.scalajs.dom.html.Span
import org.scalajs.dom.raw._
import org.scalajs.dom.{DragEvent, Event}
import org.scalatest.{FreeSpec, Matchers}

class Issue113 extends FreeSpec with Matchers {
  "name clash should be avoided" in {
    val dialog = dialogUI("id")
    dialog.watch()
    dialog.get.outerHTML should be("""<aside id="id">
    <div>
      <fieldset>
      </fieldset>
    </div>
  </aside>""")
  }

  def x = {
    implicitly[Mount[Span, Binding[String]]]
  }

  @dom
  def listBookmarks: Binding[HTMLDivElement] =
    <div>
      <ul>
        <li>
          <span ondragstart={e: DragEvent => }>
            <i>bookmark</i>
          </span> <button onclick={_: Event => }>
          <i>delete</i>
        </button>
          <button onclick={_: Event => }>
            <i>edit</i>
          </button>
        </li>
      </ul>
    </div>

  @dom
  def dialogUI(theId: String): Binding[HTMLElement] = <aside id={theId}>
    <div>
      <fieldset>
      </fieldset>
    </div>
  </aside>

}
