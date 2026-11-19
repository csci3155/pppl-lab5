package jsy.tester
import java.io.File
import jsy.util.JsyApplication
import org.scalatest.*
import flatspec.*

/*
 * A ScalaTest interface for running system tests.
 */
class JavascriptyTester(rootPrefix: Option[String], testDirectory: String, jsy: JsyApplication) extends AnyFlatSpec {
  val testResources = s"src${File.separator}test${File.separator}resources"
  val testPrefix = rootPrefix match {
    case None => testResources
    case Some(p) => s"${p}${File.separator}${testResources}"
  }
  val testPath = s"${testPrefix}${File.separator}${testDirectory}"
  jsy.test(new File(testPath)) { case (in,ans,assertion) =>
    val testLabel = in.getName.stripSuffix(".jsy")
    registerTest(s"eval ${testLabel}") {
      val (b: Boolean, msg: String) = assertion
      if (!b) fail(msg)
    }
  }
}
