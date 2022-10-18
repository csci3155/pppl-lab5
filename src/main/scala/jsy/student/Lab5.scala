package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._
  
  /*
   * CSCI 3155: Lab 5
   * Reference Implementation
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

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = {
      def r(e: Expr) = ren(env, e)
      e match {
        case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
        case Print(e1) => r(e1) map { e1p => Print(e1p) }
        case Unary(uop, e1) => r(e1) map { e1p => Unary(uop, e1p) }
        case Binary(bop, e1, e2) =>
          r(e1) flatMap { e1p =>
            r(e2) map { e2p =>
              Binary(bop, e1p, e2p)
            }
          }
        case If(e1, e2, e3) => r(e1) flatMap { e1p =>
          r(e2) flatMap { e2p =>
            r(e3) map { e3p => If(e1p, e2p, e3p) }
          }
        }
        case Var(x) => doreturn(Var(env.getOrElse(x, x)))
        case Decl(mode, x, e1, e2) => fresh(x) flatMap { xp =>
          r(e1) flatMap { e1p =>
            val envp = extend(env, x, xp)
            ren(envp, e2) map { e2p => Decl(mode, xp, e1p, e2p) }
          }
        }

        case Function(p, params, retty, e1) => {
          val w: DoWith[W,(Option[String], Map[String,String])] = p match {
            case None => doreturn( (p, env) )
            case Some(x) => fresh(x) map { xp => (Some(xp), env + (x -> xp)) }
          }
          w flatMap { case (pp, envp) =>
            params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil, envp)) ) {
              case ((x,mty), acc) => acc flatMap { case (params, env) =>
                fresh(x) map { xp => ((xp,mty) :: params, env + (x -> xp)) }
              }
            } flatMap { case (paramsp, envpp) =>
              ren(envpp, e1) map { e1p => Function(pp, paramsp, retty, e1p) }
            }
          }
        }

        case Call(e1, args) => r(e1) flatMap { e1p =>
          mapWith(args) { e => r(e) } map { argsp => Call(e1p, argsp) }
        }

        case Obj(fields) => mapWith(fields) { case (f, e) => r(e) map { ep => (f, ep) } } map { fieldsp => Obj(fieldsp) }
        case GetField(e1, f) => r(e1) map { e1p => GetField(e1p, f) }

        case Assign(e1, e2) => r(e1) flatMap { e1p => r(e2) map { e2p => Assign(e1p,e2p) } }

        /* Should not match: should have been removed */
        case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
      }
    }
    ren(env, e)
  }

  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { _ =>
      doget flatMap { i =>
        val xp = "x" + i
        doput(i + 1) map { _ => xp}
      }
    }
    val (_, r) = rename(empty, e)(fresh)(0)
    r
  }

  /*** Helper: map and mapFirst to DoWith ***/

  /* map with an operator returning a DoWith */
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) { (h, dwacc) =>
      dwacc flatMap { lacc => f(h) map { b => b :: lacc } }
    }
  }

  /* map with an operator returning a DoWith */
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]]( doreturn(Map.empty) ) { (h, dwacc) =>
      dwacc flatMap { macc => f(h) map { cd => macc + cd } }
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn( Nil )
    case h :: t => f(h) match {
      case None =>
        mapFirstWith(t)(f) map { tp => h :: tp }
        /* for (tp <- mapFirstWith(f)(t)) yield h :: tp */
      case Some(withhp) =>
        withhp map { hp => hp :: t }
        /* for (hp <- withhp) yield hp :: t */
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Transformation ***/

  /* Transformation visitor. */
  def transformVisitor[Env](visitant: (Env => Expr => Expr) => Env => PartialFunction[Expr, Expr])(env: Env)(e: Expr): Expr = {
    def loop(env: Env)(e: Expr): Expr = {
      val tr: Expr => Expr = loop(env)
      val f = visitant(loop)(env).orElse[Expr,Expr] {
        case Var(_) | N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
        case Print(e1) => Print(tr(e1))
        case Unary(uop, e1) => Unary(uop, tr(e1))
        case Binary(bop, e1, e2) => Binary(bop, tr(e1), tr(e2))
        case If(e1, e2, e3) => If(tr(e1), tr(e2), tr(e3))
        case Decl(mode, y, e1, e2) => Decl(mode, y, tr(e1), tr(e2))
        case Function(p, paramse, retty, e1) => Function(p, paramse, retty, tr(e1))
        case Call(e1, args) => Call(tr(e1), args map tr)
        case Obj(fields) => Obj(fields map { case (f: String, t: Expr) => (f, tr(t)) })
        case GetField(e1, f) => GetField(tr(e1), f)
        case Assign(e1, e2) => Assign(tr(e1), tr(e2))
        case InterfaceDecl(tvar, t, e1) => InterfaceDecl(tvar, t, tr(e1))
      }
      val r = f(e)
      /* Patch up positions for error messages. */
      r setPos e.pos
    }
    loop(env)(e)
  }

  def transformVisitorSimple(visitant: (Expr => Expr) => PartialFunction[Expr, Expr])(e: Expr): Expr = {
    def myvisitant(tr: Unit => Expr => Expr): Unit => PartialFunction[Expr,Expr] = { _ => visitant(tr(())) }
    transformVisitor[Unit](myvisitant)(())(e)
  }

  def transformTypVisitor[Env](visitant: (Env => Typ => Typ) => Env => PartialFunction[Typ, Typ])(env: Env)(t: Typ): Typ = {
    def loop(env: Env)(t: Typ): Typ = {
      val tr: Typ => Typ = loop(env)
      val f = visitant(loop)(env).orElse[Typ,Typ] {
        case TNumber | TBool | TString | TUndefined | TNull | TVar(_) => t
        case TFunction(params, rt) =>
          val paramsp = params.map { case (x, MTyp(m,t)) => (x, MTyp(m,tr(t))) }
          TFunction(paramsp, tr(rt))
        case TObj(fields) => TObj(fields map { case (f: String,t: Typ) => (f, tr(t)) })
        case TInterface(tvar, t1) => TInterface(tvar, tr(t1))
      }
      f(t)
    }
    loop(env)(t)
  }

  def transformTypVisitorSimple(visitant: (Typ => Typ) => PartialFunction[Typ, Typ])(t: Typ): Typ = {
    def myvisitant(tr: Unit => Typ => Typ): Unit => PartialFunction[Typ,Typ] = { _ => visitant(tr(())) }
    transformTypVisitor[Unit](myvisitant)(())(t)
  }

  /* Substitute in type t replacing uses of type variable tvar with type tp */
  def typSubstitute(t: Typ, tp: Typ, tvar: String): Typ = {
    def subst(tr: Typ => Typ): PartialFunction[Typ,Typ] = {
      case TVar(tvarp) => if (tvar == tvarp) tp else t
      case TInterface(tvarp, t1) =>
        if (tvar == tvarp) t // tvar shadowed by tvarp
        else TInterface(tvarp, tr(t1))
    }
    transformTypVisitorSimple(subst)(t)
  }

  /* Substitute in an expression e all uses of type variable tvar with type tp */
  def typSubstituteExpr(tp: Typ, tvar: String, e: Expr): Expr = {
    def tysubst(t: Typ): Typ = typSubstitute(t, tp, tvar)
    def subst(tr: Expr => Expr): PartialFunction[Expr, Expr] = {
      case Unary(Cast(t), e1) => Unary(Cast(tysubst(t)), tr(e1))
      case Function(p, params, retty, e1) =>
        val paramsp = params map { case (x, MTyp(m,t)) => (x, MTyp(m,tysubst(t))) }
        Function(p, paramsp, retty map tysubst, tr(e1))
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException
    }
    transformVisitorSimple(subst)(e)
  }

  /*** Casting ***/
  
  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    case (TNull, TObj(_)) => true
    case (_, _) if (t1 == t2) => true
    case (TObj(fields1), TObj(fields2)) => {
      (fields2 forall { case (f2, t2) => fields1.get(f2) match { case Some(t1) => t1 == t2 case None => false } }) ||
      (fields1 forall { case (f2, t2) => fields2.get(f2) match { case Some(t1) => t1 == t2 case None => false } })
    }
    case (TInterface(tvar, t1p), _) => castOk(typSubstitute(t1p, t1, tvar), t2)
    case (_, TInterface(tvar, t2p)) => castOk(t1, typSubstitute(t2p, t2, tvar))
    case _ => false
  }
  
  /*** Type Inference ***/

  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = m match {
    case MRef => isLExpr(e)
    case _ => true
  }
    
  def typeof(env: TEnv, e: Expr): Typ = {
    def typ(e1: Expr) = typeof(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)
    def check[T](e1: Expr)(cases: PartialFunction[Typ,T]): T = {
      val errpfun: PartialFunction[Typ,T] = { case tgot => err(tgot, e1) }
      (cases orElse errpfun)(typ(e1))
    }

    e match {
      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) =>
        val MTyp(_, t) = env(x)
        t
      case Unary(Neg, e1) => check(e1){
        case TNumber => TNumber
      }
      case Unary(Not, e1) => check(e1){
        case TBool => TBool
      }
      case Binary(Plus, e1, e2) => check(e1){
        case TNumber => check(e2){ case TNumber => TNumber }
        case TString => check(e2){ case TString => TString }
      }
      case Binary(Minus|Times|Div, e1, e2) => check(e1){
        case TNumber => check(e2){ case TNumber => TNumber }
      }
      case Binary(Eq|Ne, e1, e2) => check(e1){
        case t1 if !hasFunctionTyp(t1) => check(e2){ case t2 if (t1 == t2) => TBool }
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => check(e1){
        case TNumber => check(e2){ case TNumber => TBool }
        case TString => check(e2){ case TString => TBool }
      }
      case Binary(And|Or, e1, e2) => check(e1){
        case TBool => check(e2){ case TBool => TBool }
      }
      case Binary(Seq, e1, e2) => typ(e1); typ(e2)
      case If(e1, e2, e3) => check(e1){
        case TBool => check(e2){
          case t2 => check(e3){ case t3 if (t2 == t3) => t2 }
        }
      }
      
      case Obj(fields) => TObj(fields map { case (f,t) => (f, typ(t)) })
      case GetField(e1, f) => check(e1){
        case tgot @ TObj(tfields) => tfields.getOrElse(f, err(tgot, e1))
      } 
      
      case Decl(m, x, e1, e2) => {
        val t1 = typ(e1)
        if (isBindex(m, e1)) typeof(env + (x -> MTyp(m, t1)), e2) else err(t1, e1)
      }
      
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(params, rt)
            env + (f -> MTyp(MConst,tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with the parameters.
        val env2 = env1 ++ params
        // Infer the type of the function body
        val t1 = typeof(env2, e1)
        tann foreach { rt => if (rt != t1) err(t1, e1) };
        TFunction(params, t1)
      }

      case Call(e1, args) => check(e1){
        case TFunction(params, tret) if (params.length == args.length) => {
          (params, args).zipped.foreach {
            case ((_, MTyp(mparami,tparami)),ei) => check(ei){
              case ti if (ti == tparami) && isBindex(mparami,ei) => ()
            }
          }
          tret
        }
      }
      
      case Assign(Var(x), e1) =>
        val t1 = typ(e1)
        env.get(x) match {
          case Some(MTyp(m, t)) if (m == MVar || m == MRef) && (t1 == t) => t1
          case _ => err(t1, e1)
        }
      case Assign(GetField(e1, f), e2) => check(e1){
        case TObj(fields) =>
          val t2 = typ(e2)
          fields.get(f) match {
            case Some(t) if (t2 == t) => t2
            case _ => err(t2, e2)
          }
      }
      case Assign(_, _) => err(TUndefined, e)
        
      case Null => TNull
      
      case Unary(Cast(t), e1) => check(e1) {
        case t1 if (castOk(t1, t)) => t
        case t1 =>
          /* println("Casting to: " + t) */
          err(t1, e1)
      }
        
      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }
  
  /*** Small-Step Interpreter ***/
  
  /* Do the operation for an inequality. */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 4 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) esub else e
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      case Function(p, params, retty, e1) =>
        if (p == Some(x) || (params exists { case (y,_) => x == y })) e else Function(p, params, retty, subst(e1))
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Obj(fields) => Obj(fields map { case (fi,ei) => (fi, subst(ei)) })
      case GetField(e1, f) => GetField(subst(e1), f)
        /***** New cases for Lab 5 */
      case Null | A(_) => e
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))
      case InterfaceDecl(tvar, t, e1) => InterfaceDecl(tvar, t, subst(e1))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(){ x => doreturn(fresh(x)) }
    }

    subst(myrename(e))
    // subst(e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst | MVar => !isValue(e)
    case MName => false
    case MRef => !isLValue(e)
  }

  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
      case MConst | MName | MRef => doreturn( e )
      case MVar => memalloc(e) map { a => Unary(Deref, a) }
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), s"stepping on a value: ${e}")
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
      case Unary(Neg, N(n1)) => doreturn( N(- n1) )
      case Unary(Not, B(b1)) => doreturn( B(! b1) )
      case Binary(Seq, v1, e2) if isValue(v1) => doreturn( e2 )
      case Binary(Plus, S(s1), S(s2)) => doreturn( S(s1 + s2) )
      case Binary(Plus, N(n1), N(n2)) => doreturn( N(n1 + n2) )
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(inequalityVal(bop, v1, v2)) )
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 == v2) )
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 != v2) )
      case Binary(And, B(b1), e2) => doreturn( if (b1) e2 else B(false) )
      case Binary(Or, B(b1), e2) => doreturn( if (b1) B(true) else e2 )
      case Binary(Minus, N(n1), N(n2)) => doreturn( N(n1 - n2) )
      case Binary(Times, N(n1), N(n2)) => doreturn( N(n1 * n2) )
      case Binary(Div, N(n1), N(n2)) => doreturn( N(n1 / n2) )
      case If(B(b1), e2, e3) => doreturn( if (b1) e2 else e3 )
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) => memalloc(e)
      case GetField(a @ A(_), f) =>
        doget map { m =>
          val vopt = m.get(a) flatMap {
            case Obj(fields) => fields.get(f)
            case _ => None
          }
          vopt.getOrElse(throw StuckError(e))
        }
        /*
        for (m <- doget) yield {
          val vopt = for {
            Obj(fields) <- m.get(a)
            v <- fields.get(f)
          } yield v
          vopt.getOrElse(throw StuckError(e))
        }
        */
      
      case Decl(mode, x, e1, e2) if !isRedex(mode, e1) =>
        getBinding(mode, e1) map { e1p => substitute(e2, e1p, x) }

      case Unary(Deref, a @ A(_)) =>
        doget map { m => m(a) }
        /* for (m <- doget) yield m(a) */

      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify { (m: Mem) => m + (a -> v) } map { _ => v }
        /* for (_ <- domodify { (m: Mem) => m + (a -> v) }) yield v */
      
      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>
        domodify { (m: Mem) =>
          val fields = m.get(a) match {
            case Some(Obj(fields)) if (fields.contains(f)) => fields
            case _ => throw StuckError(e)
          }
          m + (a -> Obj(fields + (f -> v)))
        } map {
          _ => v
        }

      case Call(v @ Function(p, params, _, e), args) => {
        val pazip = params zip args
        if (pazip.forall { case ((_, MTyp(mi, _)), ei) => !isRedex(mi, ei) }) {
          val dwep = pazip.foldRight( doreturn(e): DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => dwacc flatMap { eacc =>
              getBinding(mi, ei) map { ei => substitute(eacc, ei, xi) }
            }
          }
          p match {
            case None => dwep
            case Some(x) => dwep map { ep => substitute(ep, v, x) }
          }
        }
        else {
          val dwpazipp = mapFirstWith(pazip) {
            case (p @ (_, MTyp(mi, _)), ei) =>
              if (isRedex(mi, ei)) Some(
                step(ei) map { eip => (p, eip) }
              )
              else None
          }
          dwpazipp map { pazipp => Call(v, pazipp map { case (_, ei) => ei }) }
        }
      }

      case Unary(Cast(t), Null) => t match {
        case TObj(_) | TInterface(_, TObj(_)) => doreturn( Null )
        case _ => throw StuckError(e)
      }
      case Unary(Cast(t), a @ A(_)) => doget map { m =>
        m.get(a) match {
          case Some(Obj(fields)) => t match {
            case TObj(fieldst) if fieldst forall { case (fi, _) => fields.contains(fi) } => a
            case TInterface(_, TObj(fieldst)) if fieldst forall { case (fi, _) => fields.contains(fi) } => a
            case _ => throw DynamicTypeError(e)
          }
          case _ => throw StuckError(e)
        }
      }
      case Unary(Cast(_), v1) if isValue(v1) => doreturn( v1 )
        
      /* Base Cases: Error Rules */
      case Unary(Deref, Null) | GetField(Null, _) | Assign(Unary(Deref, Null), _) | Assign(GetField(Null, _), _) => throw NullDereferenceError(e)
        
      /* Inductive Cases: Search Rules */
      case Print(e1) =>
        step(e1) map { e1p => Print(e1p) }
        /* for (e1p <- step(e1)) yield Print(e1p) */
      case Unary(uop, e1) =>
        step(e1) map { e1p => Unary(uop, e1p) }
        /* for (e1p <- step(e1)) yield Unary(uop, e1p) */
      case Binary(bop, v1, e2) if isValue(v1) =>
        step(e2) map { e2p => Binary(bop, v1, e2p) }
        /* for (e2p <- step(e2)) yield Binary(bop, v1, e2p) */
      case Binary(bop, e1, e2) =>
        step(e1) map { e1p => Binary(bop, e1p, e2) }
        /* for (e1p <- step(e1)) yield Binary(bop, e1p, e2) */
      case If(e1, e2, e3) =>
        step(e1) map { e1p => If(e1p, e2, e3) }
        /* for (e1p <- step(e1)) yield If(e1p, e2, e3) */
      case Obj(fields) => fields find { case (_, ei) => !isValue(ei) } match {
        case Some((fi,ei)) =>
          step(ei) map { eip => Obj(fields + (fi -> eip)) }
          /* for (eip <- step(ei)) yield Obj(fields + (fi -> eip)) */
        case None => throw StuckError(e)
      }
      case GetField(e1, f) =>
        step(e1) map { e1p => GetField(e1p, f) }
        /* for (e1p <- step(e1)) yield GetField(e1p, f) */
        
      case Decl(mode, x, e1, e2) =>
        step(e1) map { e1p => Decl(mode, x, e1p, e2) }
        /* for (e1p <- step(e1)) yield Decl(mut, x, e1p, e2) */
      case Assign(e1, e2) if isLValue(e1) =>
        step(e2) map { e2p => Assign(e1, e2p) }
        /* for (e2p <- step(e2)) yield Assign(e1, e2p) */
      case Assign(e1, e2) =>
        step(e1) map { e1p => Assign(e1p, e2) }
        /* for (e1p <- step(e1)) yield Assign(e1p, e2) */
        
      case Call(e1, args) =>
        step(e1) map { e1p => Call(e1p, args) }
        /* for (e1p <- step(e1)) yield Call(e1p, args) */

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr = {
    type Env = Map[String, Typ]
    def removeFromTyp(env: Env, t: Typ): Typ = {
      def tyrm(tr: Env => Typ => Typ)(env: Env): PartialFunction[Typ,Typ] = {
        case TVar(tvar) => env.getOrElse(tvar, throw new IllegalArgumentException("Unknown type name %s.".format(tvar)))
        /* Should never match because introduced in this pass. */
        case TInterface(_, _) => throw new IllegalArgumentException("Gremlins: Encountered TInterface in removeInterfaceDecl.")
      }
      transformTypVisitor(tyrm)(env)(t)
    }
    def removeFromExpr(env: Env, e: Expr): Expr = {
      def rm(tr: Env => Expr => Expr)(env: Env): PartialFunction[Expr,Expr] = {
        def tyrm(t: Typ): Typ = removeFromTyp(env, t)
        val r: Expr => Expr = tr(env)
        val f: PartialFunction[Expr,Expr] = {
          case Unary(Cast(t), e1) => Unary(Cast(tyrm(t)), r(e1))
          case Function(p, params, tann, e1) =>
            val paramsp = params map { case (x, MTyp(m,t)) => (x, MTyp(m,tyrm(t))) }
            Function(p, paramsp, tann map tyrm, r(e1))
          case InterfaceDecl(tvar, t, e1) =>
            val tp = TInterface(tvar, removeFromTyp(env + (tvar -> TVar(tvar)), t))
            tr(env + (tvar -> tp))(e1)
        }
        f
      }
      transformVisitor(rm)(env)(e)
    }
    removeFromExpr(Map.empty, e)
  }

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}