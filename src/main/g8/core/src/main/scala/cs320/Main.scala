package cs320

import org.jline.reader
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedString
import reader.{LineReader, LineReaderBuilder, EndOfFileException, EOFError, UserInterruptException}

import scala.Console.{MAGENTA => M, CYAN => C, YELLOW => Y, RED => R, RESET}

object Main {

  val name = "X-FIBER"
  val prompt = s"\n$M$name>$RESET "
  val newLinePrompt = " " * (name.length + 2)

  def main(args: Array[String]): Unit = args match {
    case Array("fuzzer") => Fuzzer.run()
    case _ =>
      val terminal = TerminalBuilder.builder.dumb(false).build()
      val reader = LineReaderBuilder.builder
        .terminal(terminal)
        .highlighter(Highlighter)
        .parser(Parser)
        .variable(LineReader.SECONDARY_PROMPT_PATTERN, "%M")
        .variable(LineReader.HISTORY_FILE, s".${name.toLowerCase}_history")
        .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
        .build()
      def strs: LazyList[String] = (
        try {
          reader.readLine(prompt)
        } catch {
          case _: EndOfFileException | _: UserInterruptException => ":q"
        }
      ) #:: strs

      println(s"Welcome to the $M$name$RESET REPL.")
      println(s"Type in :q, :quit, or the EOF character to terminate the REPL.")

      for (str <- strs.takeWhile(s => !eof(s)) if str.trim.nonEmpty) {
        val opt = lift {
          val expr = Expr(str)
          println(s"  ${C}Parsed:$RESET $expr")
          expr
        }

        for (expr <- opt) {
          lift {
            val result = Implementation.interp(expr)
            println(s"  ${C}Result:$RESET ${Value.show(result)}")
          }
        }
      }
  }

  def eof(str: String): Boolean = str == ":quit" || str == ":q"

  def lift[T](res: => T): Option[T] = try {
    Some(res)
  } catch {
    case ParsingError(msg) =>
      println(s"  Parsing failed. $msg")
      None
    case e: Throwable =>
      e.printStackTrace()
      None
  }
}

object Highlighter extends reader.Highlighter {

  def highlight(reader: LineReader, buf: String): AttributedString = {
    if (buf.isEmpty) return AttributedString.fromAnsi("")

    val colorAt = Array.fill(buf.length)(RESET)
    def highlightRange(from: Int, to: Int, color: String) =
      java.util.Arrays.fill(colorAt.asInstanceOf[Array[AnyRef]], from, to, color)

    var i = 0
    def check(l: Int): Boolean =
      (i == 0 || !buf(i - 1).isLetterOrDigit) &&
      (i + l == buf.length || !buf(i + l).isLetterOrDigit)
    while (i < buf.length) {
      val (l, c) = Expr.keywords.find(x =>
        i + x.length <= buf.length &&
        x == buf.substring(i, i + x.length)
      ) match {
        case Some(x) if check(x.length) =>
          val c = if (x == "true" || x == "false" || x == "Nil") R else Y
          (x.length, c)
        case _ =>
          val l = buf.substring(i).takeWhile(_.isDigit).length
          if (l != 0 && check(l)) (l, R) else (1, RESET)
      }
      highlightRange(i, i + l, c)
      i += l
    }

    val highlighted = new StringBuilder()
    for (idx <- colorAt.indices) {
      val prev = if (idx == 0) RESET else colorAt(idx - 1)
      val curr = colorAt(idx)
      if (curr != prev)
        highlighted.append(curr)
      highlighted.append(buf(idx))
    }
    if (colorAt.last != RESET)
      highlighted.append(RESET)
    AttributedString.fromAnsi(highlighted.toString)
  }

  def setErrorPattern(errorPattern: java.util.regex.Pattern): Unit = ()
  def setErrorIndex(errorIndex: Int): Unit = ()
}

class ParsedLine(
  val cursor: Int, val line: String, val word: String, val wordCursor: Int
) extends reader.ParsedLine {
  def wordIndex = -1
  def words = java.util.Collections.emptyList
}

object Parser extends reader.Parser {
  import reader.Parser.ParseContext

  def parse(input: String, cursor: Int, context: ParseContext): ParsedLine = {
    def default = new ParsedLine(cursor, input, "", 0)
    def acceptLine = {
      val i = input.trim
      def isEmpty = i.isEmpty
      def isLastLine = !input.substring(cursor).contains(System.lineSeparator)
      def isBalanced =
        i.count(_ == '(') == i.count(_ == ')') &&
        i.count(_ == '{') == i.count(_ == '}') &&
        i.count(_ == '[') == i.count(_ == ']')
      val noLast = Set('-', '!', '+', '*', '/', '%', '=', '<', '>', '&', '|', ':', '.', ';')
      def isFinished = !noLast(i.last)
      isEmpty || isLastLine && isBalanced && isFinished
    }
    context match {
      case ParseContext.ACCEPT_LINE if acceptLine => default
      case ParseContext.COMPLETE => default
      case _ => throw new EOFError(-1, -1, "", Main.newLinePrompt)
    }
  }
}
