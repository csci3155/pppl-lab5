package jsy.lab5

import scala.collection.immutable.SortedMap
import jsy.lab5.ast._
import jsy.lab5.Parser.parse
import jsy.tester.JavascriptyTester
import jsy.util.DoWith
import jsy.util.DoWith._
import org.scalatest._
import flatspec._
import Lab5._
import java.{util => ju}

class Lab5StudentSpec extends AnyFlatSpec {
}

class Lab5Spec extends AnyFlatSpec {

  "DoNeg" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = Unary(Neg, e1)
    assertResult( N(-5) ) {
      val (_, r) = step(e2)(memempty)
      r
    }
  }

  "DoPlus" should "Perform addition of two numbers" in {
    val e1 = N(1)
    val e2 = N(2)
    val e = Binary(Plus, e1, e2)
    assertResult( N(3) ) {
      val (_, r) = step(e)(memempty)
      r
    }
  }

  "Decl" should "Evaluate Decl" in {
    val e1 = Binary(Plus, N(1), N(3))
    val e2 = Var("x")
    val e = Decl(MVar, "x", Binary(Plus, e1, N(1)), e2)
    val ep = Decl(MVar, "x", Binary(Plus, N(4), N(1)), e2)
    assertResult( ep ) {
      val (_, r) = step(e)(memempty)
      r
    }
  }

  "DoDeref" should "Deref a Reference" in {
    val e = Unary(Deref, A(1))
    assertResult(N(1)) {
      val (_, r) = step(e)(memempty + (A(1) -> N(1)))
      r
    }
  }

  "TypeofNeg" should "Return type TNumber" in {
    val e1 = Binary(Plus, N(1), N(2))
    val e = Unary(Neg, e1)
    assertResult(TNumber) {
      hastype(Map.empty, e)
    }
  }

  "TypeofAnd" should "Return type TBool" in {
    val e1 = B(false)
    val e2 = Unary(Not, e1)
    val e = Binary(And, e1, e2)
    assertResult(TBool) {
      hastype(Map.empty, e)
    }
  }

  "TypeofAssign" should "Return type TNumber" in {
    val mp = Map("x" -> new MTyp(MVar, TNumber))
    val e1 = Binary(Plus, N(1), N(2))
    val e = Assign(Var("x"), e1)
    assertResult(TNumber) {
      hastype(mp, e)
    }
  }
 
  // "mapFirstDoWith" should "map the first element where f returns Some" in {
  //    val l1 = List(1, 2, -3, 4, -5)
  //    val gold1 = List(1, 2, 3, 4, -5)
  //    def dowith[W]: DoWith[W,List[Int]] = mapFirstWith(l1) { (i: Int) => if (i < 0) Some(doreturn(-i)) else None }
  //    assertResult((true,gold1)) { dowith(true) }
  //    assertResult((42,gold1)) { dowith(42) }
  // }

  // "mapWith(List)" should "map the elements of a list in a DoWith" in {
  //   val l = List(1, 2, 3, 4, 5)
  //   val r1 = l.map { i => i + 1 }

  //   def dowith1[W]: DoWith[W,List[Int]] = mapWith(l) { i: Int => doreturn(i + 1) }
  //   assertResult((true,r1)) { dowith1(true) }
  //   assertResult((42,r1)) { dowith1(42) }

  //   assertResult((2 * l.length + 1, r1)) {
  //     val dw: DoWith[Int,List[Int]] = mapWith(l) { i: Int =>
  //       domodify[Int](s => s + 2) map { _ => i + 1 }
  //     }
  //     dw(1)
  //   }
  // }

  // "rename" should "rename in a DoWith" in {
  //   val e1 = parse("const a = 1 + a; a")
  //   val e1p = parse("const aa = 1 + a; aa")

  //   assertResult((1,e1p)) {
  //     rename(empty, e1){ x => domodify[Int](n => n + 1) map { _ => x + x } }(0)
  //   }
  // }

  // "CastOkNull" should "perform CastOkNull" in {
  //   assertResult(true) {
  //     castOk(TNull, TObj(SortedMap.empty))
  //   }
  // }

}


// The next bit of code runs a test for each .jsy file in src/test/resources/lab5.
// The test expects a corresponding .ans file with the expected result.
class Lab5JsyTests extends JavascriptyTester(None, "lab5", Lab5)
class Lab5XCJsyTests extends JavascriptyTester(None, "lab5xc", Lab5)