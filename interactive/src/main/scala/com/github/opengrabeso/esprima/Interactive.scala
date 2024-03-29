package com.github.opengrabeso.esprima

import java.awt.event.{InputEvent, KeyEvent}
import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}
import java.util.prefs.Preferences

import javax.swing.{SwingUtilities, UIManager}
import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea, SyntaxConstants}
import org.fife.ui.rtextarea.RTextScrollPane

import scala.util.Try
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.event.ValueChanged

// copied and adapted from ScalaFromJS

//noinspection ForwardReference
object Interactive extends SimpleSwingApplication {

  def prefs: Preferences = Preferences.userRoot().node(getClass.getPackage.getName.toLowerCase)

  override def startup(args: scala.Array[scala.Predef.String]): Unit = {

    assert(SwingUtilities.isEventDispatchThread)

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    super.startup(args)
  }

  def top: MainFrame = new MainFrame {

    title = "Esprima interactive parser"

    minimumSize = new Dimension(800, 600)

    class MyTextArea(edit: Boolean) extends TextArea {
      override lazy val peer: RSyntaxTextArea = new RSyntaxTextArea("", 40, 80) with SuperMixin
      minimumSize = new Dimension(300, 100)
      editable = edit

      if (edit) {
        peer.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_TYPESCRIPT)
      } else {
        peer.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_NONE)
      }
      peer.setHighlightSecondaryLanguages(false)
      peer.setCodeFoldingEnabled(true)
    }

    class MyScrollPane(panel: TextComponent) extends ScrollPane(panel) {
      override lazy val peer: RTextScrollPane = new RTextScrollPane(panel.peer, false) with SuperMixin
    }

    var output: OutputType = OutputType.AST

    private def mySplit(o: Orientation.Value, w: Double)(l: Component, r: Component): SplitPane = new SplitPane(o, l, r) {
      resizeWeight = w
    }
    val result = new MyTextArea(false)
    val input = new MyTextArea(true)
    val statusBar = new Label

    def switchOutput(o: OutputType): Unit = {
      output = o
      BackgroundConversion.process(input.text)
    }

    contents = new BorderPanel {
      layout += mySplit(Orientation.Vertical, 0.5)(new MyScrollPane(input), new MyScrollPane(result)) -> Center
      layout += statusBar -> South
    }

    loadSession()

    listenTo(input)

    reactions += {
      case ValueChanged(`input`) =>
        val text = input.text
        println("Input changed")
        BackgroundConversion.process(text)
        saveSession()
    }

    BackgroundConversion.process(input.text)

    def saveSession(): Unit = {
      try {
        prefs.put("version", "0")
        prefs.put("input", input.text)
      } catch {
        case ex: Exception =>
        // prefs have limited size - can throw exception
      }
    }


    def loadSession(): Unit = {
      val version = prefs.get("version", "")
      if (version.nonEmpty) {
        val loaded = prefs.get("input", "")
        input.text = loaded
      }
    }



    /*
    onCloseRequest = handle {
      BackgroundConversion.process(null)
    }
     */

    object BackgroundConversion {
      // use actors instead?
      val inputs = new ConcurrentLinkedQueue[(String, Long)]
      val waitForInput = new Semaphore(0, true)

      def process(in: String) = {
        inputs.add(in -> System.currentTimeMillis())
        waitForInput.release()
      }

      def background(): Unit = {

        def now() = System.currentTimeMillis()

        while (true) {
          waitForInput.acquire()
          var lastInput: (String, Long) = inputs.poll()
          // empty the queue
          while (waitForInput.tryAcquire() && lastInput._1 != null) {
            lastInput = inputs.poll()
          }
          if (lastInput._1 == null) return
          val start = now()
          val resultValue = Try {
            // almost all JS can be parsed as TS and we want to parse TS as well
            // allow explicit TS/JS selection (this is sometimes important for testing)
            val firstLine = lastInput._1.split("\n", 1)(0).toLowerCase

            object InteractiveOptions extends Parser.Options {
              range = true
              attachComment = true
              tolerant = true
              typescript = !firstLine.startsWith("//js") && !firstLine.startsWith("//javascript")
              sourceType = "module" // allow exports
              async = true
            }
            Esprima.parse(lastInput._1, InteractiveOptions)
          }
          val duration = now() - start

          // avoid overwriting newer result
          SwingUtilities.invokeLater {
            new Runnable {
              override def run() =
                println(s"Duration $duration")
              resultValue.map { ast =>
                result.text = output.output(ast)
                statusBar.text = s"Conversion duration $duration ms"
              }.failed.map { ex =>
                statusBar.text = s"Conversion duration $duration ms, error ${ex.toString}"
              }

            }
          }
        }
      }

      val backgroundThread = new Thread(new Runnable {
        def run() = background()
      })
      backgroundThread.start()
    }


    menuBar = new MenuBar {
      contents ++= Seq(
        new Menu("Output") {
          contents ++= Seq(
            new MenuItem(new ActionWithCode("Use toString", {switchOutput(OutputType.ToString)}, KeyEvent.VK_1, InputEvent.CTRL_DOWN_MASK)),
            new MenuItem(new ActionWithCode("Use AST walker", {switchOutput(OutputType.AST)}, KeyEvent.VK_2, InputEvent.CTRL_DOWN_MASK))
          )
        }
      )
    }

    def showStatus(): Unit = {
      statusBar.text = ""
    }

    showStatus()

  }
}
