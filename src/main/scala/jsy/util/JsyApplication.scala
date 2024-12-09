package jsy.util

import java.io.{ByteArrayOutputStream, File}
import java.nio.file.Files

trait JsyApplication {
  var debug = false /* set to false to disable debugging output */
  var keepGoing = true /* set to true to keep going after exceptions */
  var maxSteps: Option[Int] = None /* set to a number to bound the number of execution steps */

  /*** Implement this method to process a file. ***/
  def processFile(file: File): Unit
  
  var anonOption = ("input",
    { case filename :: Nil => new File(filename) }: PartialFunction[List[String], File],
    "A file containing a JavaScripty program or a directory with .jsy files.")
    
  var flagOptions = List(
    ("debug", options.SetBool(b => debug = b, Some(b => debug == b)), "debugging"),
    ("keep-going", options.SetBool(b => keepGoing = b, Some(b => keepGoing == b)), "keep going after exceptions"),
    ("bound-steps", options.SetInt(
        { i => maxSteps = i },
        Some({ 
          case true => maxSteps map { i => ": %d".format(i) }
          case false => if (maxSteps == None) Some("") else None
        })),
     "bound for maximum number of execution steps before aborting")
  )
  
  def handle[T](default: T)(e: => T): T =
    if (!keepGoing) e
    else try e catch {
      case exn: Throwable => println(exn.toString); default
    }

  def testJsy(file: File)(k: (File, File, => (Boolean, String)) => Unit): Unit = {
    val jsyext = """[.]jsy$""".r
    val ans: File = new File(jsyext.replaceAllIn(file.getPath, ".ans"))
    k(file, ans, {
      if (!ans.exists()) {
        (false, s"Expected output file ${ans} does not exist.")
      }
      else {
        val outstream = new ByteArrayOutputStream()
        Console.withOut(outstream)(handle(()){ processFile(file)})

        val encoding = java.nio.charset.StandardCharsets.UTF_8
        val ansstring = new String(Files.readAllBytes(ans.toPath), encoding)

        outstream.flush()
        val outstring = new String(outstream.toString(encoding.toString))
        outstream.close()

        (ansstring == outstring, s"Computed output does not match expected output.\nComputed:\n${outstring}\nExpected:\n${ansstring}")
      }
    })
  }
  
  def isJsy(file: File): Boolean = {
    val jsyext = """[.]jsy$""".r
    jsyext findFirstIn file.getName match {
      case Some(_) => true
      case None => false
    }
  }

  def doFile(doit: File => Unit, file: File) = {
    def loop(file: File): Unit = {
      if (file.isFile) {
        doit(file)
      }
      else if (file.isDirectory) {
        file.listFiles filter { f => f.isDirectory || isJsy(f) } foreach loop
      }
      else {
        throw new IllegalArgumentException("File %s does not exist.".format(file))
      }
    }
    loop(file)
  }

  def test(fileordir: File)(k: (File, File, => (Boolean, String)) => Unit): Unit =  {
    doFile({ f => testJsy(f)(k) }, fileordir)
  }

  def main(args: Array[String]): Unit =  {
    val opts = new options.Options("jsy", flagOptions, anonOption)
    val file: File = opts.process(args)
    doFile(processFile, file)
  }
}

object options {
  sealed abstract class Spec
  case class SetBool(setter: Boolean => Unit, default: Option[Boolean => Boolean]) extends Spec
  case class SetInt(setter: Option[Int] => Unit, default: Option[Boolean => Option[String]]) extends Spec

  class Options[T](program: String, specs: List[(String, Spec, String)], anon: (String, PartialFunction[List[String], T], String)) {
    val opts: Map[String, List[String] => Option[List[String]]] =
      specs.foldLeft[Map[String, List[String] => Option[List[String]]]](Map.empty)((acc, spec) => spec match {
        case (name, SetBool(setter, _), _) =>
          acc +
          (("--" + name) -> { (t: List[String]) => setter(true); Some(t) }) +
          (("--no-" + name) -> { (t: List[String]) => setter(false); Some(t) })
        case (name, SetInt(setter, _), _) =>
          acc +
          (("--" + name) -> { (t: List[String]) => t match {
            case arg :: t =>
              val argp: Int = try arg.toInt catch { case _: Throwable => usageErr() }
              setter(Some(argp)); Some(t)
            case _ => usageErr()
          } }) +
          (("--no-" + name) -> { (t: List[String]) => setter(None); Some(t) })
      })
      
    val (anonName, anonDo, anonDesc) = anon
      
    val nameWidth: Int = opts.foldLeft(anonName.length){ case (acc, (n, _)) => acc max (n.length + 5) }
    
    def padRight(s: String, w: Int): String = {
      val nspaces = (w - s.length) max 0
      s + (" " * nspaces)
    }
    
    def optline(name: String, text: String): String = {
      "%-2s".format("") + padRight(name, nameWidth) + "  %s%n".format(text)
    }
      
    val descriptions: String = {
      (specs foldRight "")((spec, acc) => spec match {
        case (name, SetBool(_, default), desc) => {
          def defaultStr(b: Boolean): String =
            default map (f => if (f(b)) " (default)" else "") getOrElse("") 
          optline("--" + name, "turn on %s".format(desc) + defaultStr(true)) +
          optline("--no-" + name, "turn off %s".format(desc) + defaultStr(false)) +
          acc
        }
        case (name, SetInt(_, default), desc) => {
          def defaultStr(b: Boolean): String = {
            val opt =
              for {
                f <- default
                s <- f(b)
              } yield " (default%s)".format(s)
            opt.getOrElse("")
          }
          optline("--" + name + " <int>", "set %s".format(desc) + defaultStr(true)) +
          optline("--no-" + name, "unset %s".format(desc) + defaultStr(false)) +
          acc
        }
      })
    }
    
    val header =
      """
Usage: %s [options] %s
        
""".format(program, anonName) +
optline(anonName, anonDesc) + """
Options:
"""

    private var currentArgs: List[String] = List()

    def usageErr(): Nothing = {
      val unprocessed = currentArgs match {
        case Nil => ""
        case _ => """
Unprocessed Arguments:
  """ + currentArgs.reduceRight({ _ + " " + _ }) + "%n".format()
      }
      print(header + descriptions + unprocessed)
      sys.exit(1)
    }
    
    def process(args: Array[String]): T = {
      def loop(l: List[String]): List[String] = {
        currentArgs = l
        l match {
          case Nil => Nil
          case h :: t => opts.get(h) match {
            case None => l
            case Some(doit) => { val tp = doit(t).getOrElse(t); loop(tp) }
          }
        }
      }
      val err: PartialFunction[List[String], T] = { case _ => usageErr() }
      (anonDo orElse err)(loop(args.toList))
    }
  }
}