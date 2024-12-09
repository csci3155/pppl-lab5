package jsy.lab5

import scala.collection.immutable.SortedMap
import jsy.lab5.Parser.parse

object Lab5 extends jsy.util.JsyApplication {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * <Your Name>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*** Type Inference ***/

  def hasloctype(env: Map[String, MTyp], e: Expr): Option[Typ] = ???

  def hasmodetype(env: Map[String, MTyp], e: Expr, d: Mode): Option[Typ] = ???

  def hastype(env: Map[String, MTyp], e: Expr): Typ = {
    /* Shortcuts for StaticTypeError. Use `err` when you can infer a type for e1 and `errnotype` when you cannot. */
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)
    def errnotype[T](e1: Expr): T = err(TUndefined, e1)

    e match {
      case Print(e1) => hastype(env, e1); TUndefined
        /***** Cases directly from before. We will minimize the test of these cases in Lab 5. */
      case N(_) => ???
      case B(_) => ???
      case Undefined => ???
      case S(_) => ???
      case Var(x) => ???
      case Unary(Neg, e1) => hastype(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) =>
        ???
      case Binary(Plus, e1, e2) =>
        ???
      case Binary(Minus|Times|Div, e1, e2) =>
        ???
      case Binary(Eq|Ne, e1, e2) =>
        ???
      case Binary(Lt|Le|Gt|Ge, e1, e2) =>
        ???
      case Binary(And|Or, e1, e2) =>
        ???
      case Binary(Seq, e1, e2) =>
        ???
      case If(e1, e2, e3) =>
        ???

      case Obj(fields) => ???
      case GetField(e1, f) => ???
      case Decl(m, x, e1, e2) => ???

      case Fun(xopt, params, tretopt, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (xopt, tretopt) match {
          /***** Add cases here *****/
          case _ => errnotype(e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = ???
        // Infer the type of the function body
        val t1 = ???
        // Check with the possibly annotated return type
        ???
      }
      case Call(e1, args) => hastype(env, e1) match {
        case TFun(params, tret) if (params.length == args.length) =>
          (params zip args).foreach {
            ???
          }
          tret
        case tgot => err(tgot, e1)
      }

        /***** New case. ***/
      case Assign(e1, e2) => ???

      /* Should not match: non-source expressions */
      case A(_) | Unary(Deref, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /* Scope-respecting substitution replacing variables `x` with `with_e` in `e`.
     Assume that the free variables of `with_e` and `e` have an empty intersection
    (to avoid free-variable capture). */
  def substitute(with_e: Expr, x: String, e: Expr): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => ???
      case Binary(bop, e1, e2) => ???
      case If(e1, e2, e3) => ???
      case Var(y) => ???
      case Decl(d, y, e1, e2) => ???
      case Fun(xopt, params, tann, e1) =>
        ???
      case Call(e1, args) => ???
      case Obj(fields) => ???
      case GetField(e1, f) => ???
      case A(_) => ???
      case Assign(e1, e2) => ???
    }
    subst(e)
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), s"stepping on a value: ${e}")
    e match {
      /* Base Cases */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
      case Unary(Neg, v1) if isValue(v1) => ???
        /***** More cases here */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) =>
        ???
      case GetField(a @ A(_), f) =>
        ???

        /***** New cases. */
      case Decl(MConst, x, v1, e2) if isValue(v1) =>
        ???
      case Decl(MVar, x, v1, e2) if isValue(v1) =>
        ???
      case Decl(MRef, x, l1, e2) if ??? => 
        ???

      case Unary(Deref, a @ A(_)) =>
        ???

      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => ??? } map { _ => ??? }

      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>
        ???

      case Call(v @ Fun(xopt, params, _, e), args) => {
        val pazip = params zip args
        val ep = pazip.foldRight(e) {
          ???
        }
        ???
      }

      /* Inductive Cases */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) => ???
        /***** More cases here */
      case GetField(e1, f) =>
        ???
      case Obj(fields) =>
        ???

      case Decl(d, x, e1, e2) =>
        ???
      case Call(e1, args) =>
        ???

        /***** New cases.  */
      case Assign(e1, e2) if ??? =>
        ???
      case Assign(e1, e2) =>
        ???

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** External Interfaces ***/

  /** Interface to run your small-step interpreter
    * and print out the steps of evaluation if debugging. */
  def iterateStep(e: Expr): Expr = {
    require(closed(e), "input Expr to iterateStep is not closed: free variables: %s".format(freeVars(e)) )
    def loop(e: Expr, n: Int): DoWith[Mem,Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e, n)
      else if (isValue(e)) doreturn( e )
      else {
        doget[Mem] flatMap { m =>
          println("## step %4d:%n##  %s%n##  %s".format(n, m, e))
          step(e) flatMap { ep => loop(ep, n + 1) }
        }
      }
    val (m,v) = loop(e, 0)(memempty)
    println("## result:%n##  %s%n##  %s".format(m, v))
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(parse(s))

  /** Interface to run your type checker. */
  def inferType(e: Expr): Typ = {
    val t = hastype(Map.empty, e)
    println(s"## type ${e} : ${pretty(t)}")
    t
  }

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

  // Interface for main
  def processFile(file: java.io.File): Unit = {
    if (debug) {
      println("# ============================================================")
      println("# File: " + file.getName)
      println("# Parsing ...")
    }

    val expr =
      handle(None: Option[Expr]) {Some{
        jsy.lab5.Parser.parseFile(file)
      }} getOrElse {
        return
      }

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Type checking %s ...".format(expr))
    }

    val welltyped = handle(false) {
      val t = inferType(expr)
      true
    }
    if (!welltyped) return

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Stepping %s ...".format(expr))
    }

    handle(()) {
      val v = iterateStep(expr)
      println(pretty(v))
    }
  }
}