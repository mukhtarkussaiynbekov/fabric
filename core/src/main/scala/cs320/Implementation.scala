package cs320

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._

    // type scheme
    type TypeScheme = (Type, List[VarT])

    // type name
    case class TypeName(name: String)

    // type environment
    type TypeEnv = Map[Any, Any]

    def lookupTenv(x: Any, tenv: TypeEnv) = tenv.getOrElse(x, error(s"free identifier: $x"))

    def substitute(unsubstitutedType: Type, substituteMap: Map[String, Type]): Type = unsubstitutedType match {
      case IntT | BooleanT | UnitT => unsubstitutedType
      case ArrowT(ptypes, rt) => ArrowT(ptypes.map(substitute(_, substituteMap)), substitute(rt, substituteMap))
      case AppT(x, targs) => AppT(x, targs.map(substitute(_, substituteMap)))
      case VarT(x) => substituteMap.getOrElse(x, unsubstitutedType)
    }

    // Copied from lecture slides the following three functions
    def mustSame(left: Type, right: Type): Type =
      if (isSame(left, right)) left
      else notype(s"$left is not equal to $right")

    def isSame(left: Type, right: Type): Boolean =
      (left, right) match {
        case (IntT, IntT) | (BooleanT, BooleanT) | (UnitT, UnitT) => true
        case (VarT(x), VarT(y)) => x == y
        case (AppT(_, targs1), AppT(_, targs2)) => (targs1.length == targs2.length) &&
          (targs1 zip targs2).foldLeft(true){(boolAcc, paramsPair) => boolAcc && isSame(paramsPair._1, paramsPair._2)}
        case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
          (p1.length == p2.length) &&
          (p1 zip p2).foldLeft(true){(boolAcc, paramsPair) => boolAcc && isSame(paramsPair._1, paramsPair._2)} && 
          isSame(r1, r2)
        case _ => false
      }

    def notype(msg: Any): Nothing = error(s"no type: $msg")

    def wellFormedCheck(t: Type, tenv: TypeEnv): Type = t match {
      case AppT(name, targs) => 
        targs.foreach(wellFormedCheck(_, tenv))
        lookupTenv(TypeName(name), tenv) match {
          case TypeDef(_, tparams, _) => if (tparams.length != targs.length) error("wrong arity") else t
          case _ => error("invalid type")
        }
      case VarT(name) => if (tenv.contains(t)) t else notype(s"$t is a free identifier")
      case IntT | BooleanT | UnitT => t
      case ArrowT(ptypes, rtype) =>
        ptypes.foreach(wellFormedCheck(_, tenv))
        wellFormedCheck(rtype, tenv)
        t
    }

    def wellFormedCheck(t: RecDef, tenv: TypeEnv): RecDef = t match {
      case Lazy(_, lt, e) =>
        wellFormedCheck(lt, tenv)
        mustSame(lt, typeCheck(e, tenv))
        t
      case RecFun(x, tparams, params, rt, b) =>
        tparams.foreach(ti => if (tenv.contains(VarT(ti))) error(s"$ti must not be in tenv"))
        val tenv0 = tparams.foldLeft(tenv){(tenvPrev, ti) => tenvPrev + (VarT(ti) -> None)}
        params.foreach(param => wellFormedCheck(param._2, tenv0))
        wellFormedCheck(rt, tenv0)
        val tenvM = params.foldLeft(tenv0){(tenvPrev, param) => tenvPrev + (param._1 -> ((param._2, Nil), false))}
        mustSame(rt, typeCheck(b, tenvM))
        t
      case TypeDef(name, tparams, variants) =>
        tparams.foreach(ti => if (tenv.contains(VarT(ti))) error(s"$ti must not be in tenv"))
        val tenvN = tparams.foldLeft(tenv){(tenvPrev, ti) => tenvPrev + (VarT(ti) -> None)}
        variants.foreach(wi => wi.params.foreach(ti => wellFormedCheck(ti, tenvN)))
        t
    }

    def typeCheck(c: Case, tenv: TypeEnv, variants: List[Variant], vars: List[String], types: List[Type]): Type = 
      if (vars.length != types.length) error("wrong arity") else {
        variants.foldLeft(None: Option[Variant]){(acc, wi) => acc match {
          case Some(_) => acc
          case None => if (wi.name == c.variant) Some(wi) else None
        }} match {
          case None => error("no matching variant")
          case Some(w) => if (w.params.length != c.names.length) error("wrong arity") else {
            val nenv = w.params.zipWithIndex.foldLeft(tenv){(prevTenv, tiWidx) => prevTenv + (c.names(tiWidx._2) -> ((substitute(tiWidx._1, (vars zip types).toMap), Nil), false))}
            typeCheck(c.body, nenv)
          }
        }
      }

    def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match {
      case Id(name, targs) =>
        targs.foreach(wellFormedCheck(_, tenv))
        lookupTenv(name, tenv) match {
          case typeSchemeWithMut: (TypeScheme, Boolean) => if (typeSchemeWithMut._1._2.length != targs.length) error("wrong arity") else substitute(typeSchemeWithMut._1._1, (typeSchemeWithMut._1._2.map(_.name) zip targs).toMap)
          case _ => error("invalid type")
        }
      case IntE(_) => IntT
      case BooleanE(_) => BooleanT
      case UnitE => UnitT
      case Add(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        IntT
      case Mul(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        IntT
      case Div(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        IntT
      case Mod(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        IntT
      case Eq(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        BooleanT
      case Lt(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        BooleanT
      case Sequence(l, r) =>
        typeCheck(l, tenv)
        typeCheck(r, tenv)
      case If(c, t, f) =>
        mustSame(typeCheck(c, tenv), BooleanT)
        mustSame(typeCheck(t, tenv), typeCheck(f, tenv))
      case Val(m, name, typ, e, b) => typ match {
        case Some(t) => 
          wellFormedCheck(t, tenv)
          val eT = typeCheck(e, tenv)
          mustSame(eT, t)
          typeCheck(b, tenv + (name -> ((eT, Nil), m)))
        case None => typeCheck(b, tenv + (name -> ((typeCheck(e, tenv), Nil), m)))
      }
      case RecBinds(defs, b) =>
        val tenvN = defs.foldLeft(tenv){(tenvPrev, di) => 
          val tenvI = di match {
            case Lazy(x, t, e) => Map(x -> ((t, Nil), false))
            case RecFun(x, tparams, params, rt, b) => Map(x -> ((ArrowT(params.map(_._2), rt), tparams.map(VarT(_))), false))
            case typeDef @ TypeDef(name, tparams, variants) => if (tenv.contains(TypeName(name))) error(s"$name must not be in the domain of initial tenv") else {
              val tvars = tparams.map(VarT(_))
              variants.foldLeft(Map(TypeName(name) -> typeDef): TypeEnv){(genvPrev, wi) =>
                if (wi.params.length == 0) genvPrev + (wi.name -> ((AppT(name, tvars), tvars), false)) else genvPrev + (wi.name -> ((ArrowT(wi.params, AppT(name, tvars)), tvars), false))
              }
            }
          }
          tenvPrev ++ tenvI
        }
        defs.foreach(wellFormedCheck(_, tenvN))
        wellFormedCheck(typeCheck(b, tenvN), tenv)
      case Fun(params, b) =>
        params.foreach(param => wellFormedCheck(param._2, tenv))
        val tenvN = params.foldLeft(tenv){(tenvPrev, pi) => tenvPrev + (pi._1 -> ((pi._2, Nil), false))}
        ArrowT(params.map(_._2), typeCheck(b, tenvN))
      case Assign(x, e) => lookupTenv(x, tenv) match {
        case (typeScheme: TypeScheme, mut: Boolean) =>
          if (typeScheme._2.length > 0) error("there must be no type parameter")
          else if (!mut) error("must be var")
          mustSame(typeScheme._1, typeCheck(e, tenv))
          UnitT
        case _ => error("not typescheme with mutation")
      }
      case App(f, args) => typeCheck(f, tenv) match {
        case ArrowT(ptypes, rt) => if (ptypes.length != args.length) error("wrong arity") else {
          (ptypes zip args).foreach(te => mustSame(te._1, typeCheck(te._2, tenv)))
          rt
        }
        case _ => error("must be a function type")
      }
      case Match(e, cases) => typeCheck(e, tenv) match {
        case AppT(x, targs) => lookupTenv(TypeName(x), tenv) match {
          case TypeDef(x, tparams, variants) => if (targs.length != tparams.length || variants.length != cases.length) error("wrong arity") else {
            cases.foldLeft(typeCheck(cases(0), tenv, variants, tparams, targs)){(accType, elem) => mustSame(accType, typeCheck(elem, tenv, variants, tparams, targs))}
          }
          case _ => error("must be a type definition")
        }
        case _ => error("must be a type application")
      }
    }

    def typeCheck(expr: Expr): Type = typeCheck(expr, Map())
  }

  object U {
    import Untyped._

    def numVop(op: (BigInt, BigInt) => BigInt, excludeZero: Boolean): (Value, Value) => IntV = (_,_) match {
      case (IntV(x), IntV(y)) => if (excludeZero && y == 0) error(s"second argument is zero") else IntV(op(x,y))
      case (x, y) => error(s"not both numbers: $x, $y")
    }

    def numVCompOp(op: (BigInt, BigInt) => Boolean): (Value, Value) => BooleanV = (_,_) match {
      case (IntV(x), IntV(y)) => BooleanV(op(x,y))
      case (x, y) => error(s"not both numbers: $x, $y")
    }

    val intVAdd = numVop(_ + _, false)
    val intVMul = numVop(_ * _, false)
    val intVDiv = numVop(_ / _, true)
    val intVMod = numVop(_ % _, true)
    val intVEq = numVCompOp(_ == _)
    val intVLt = numVCompOp(_ < _)

    type Store = Map[Addr, Value]

    def malloc(sto: Store): Addr = (sto.keySet + 0).max + 1

    def malloc(sto: Store, env: Env): Addr = ((sto.keySet + 0) ++ env.values).max + 1

    def lookupEnv(x: String, env: Env) = env.getOrElse(x, error(s"free identifier: $x"))
    def lookupStore(addr: Addr, sto: Store) = sto.getOrElse(addr, error(s"free identifier: $addr"))

    def interp(expr: Expr, env: Env, sto: Store): (Value, Store) = expr match {
      case Id(x) =>
        val addr = lookupEnv(x, env)
        val v = lookupStore(addr, sto)
        v match {
          case ExprV(le, lenv) =>
            val (lv, ls) = interp(le, lenv, sto)
            (lv, ls + (addr -> lv))
          case _ => (v, sto)
        }
      case IntE(n) => (IntV(n), sto)
      case BooleanE(b) => (BooleanV(b), sto)
      case UnitE => (UnitV, sto)
      case Add(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (intVAdd(lv, rv), rs)
      case Mul(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (intVMul(lv, rv), rs)
      case Div(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (intVDiv(lv, rv), rs)
      case Mod(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (intVMod(lv, rv), rs)
      case Eq(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (intVEq(lv, rv), rs)
      case Lt(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (intVLt(lv, rv), rs)
      case Sequence(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        interp(r, env, ls)
      case If(c, t, f) =>
        val (cv, cs) = interp(c, env, sto)
        cv match {
          case BooleanV(cb) => if (cb) interp(t, env, cs) else interp(f, env, cs)
          case _ => error("condition must result in a boolean")
        }
      case Val(x, e, b) =>
        val (ev, s) = interp(e, env, sto)
        val nAddr = malloc(sto)
        interp(b, env + (x -> nAddr), s + (nAddr -> ev))
      case RecBinds(defs, b) =>
        val envEmpty: Env = Map()
        val envN = defs.foldLeft(envEmpty){(envPrev, di) => 
          val envi = di match {
            case Lazy(x, e) =>
              val naddr = malloc(sto, envPrev ++ env)
              envEmpty + (x -> naddr)
            case RecFun(x, params, b) => 
              val naddr = malloc(sto, envPrev ++ env)
              envEmpty + (x -> naddr)
            case TypeDef(variants) =>
              variants.foldLeft(envEmpty){(envarPrev, wi) =>
                val naddr = malloc(sto, envPrev ++ envarPrev ++ env)
                envarPrev + (wi.name -> naddr)
              }
          }
          envPrev ++ envi
        }
        val nenv = env ++ envN
        val stoEmpty: Store = Map()
        val nsto = defs.foldLeft(sto){(stoPrev, di) =>
          val stoi = di match {
            case Lazy(x, e) =>
              stoEmpty + (lookupEnv(x, nenv) -> ExprV(e, nenv))
            case RecFun(x, params, b) => 
              stoEmpty + (lookupEnv(x, nenv) -> CloV(params, b, nenv))
            case TypeDef(variants) =>
              variants.foldLeft(stoEmpty){(storePrev, wi) =>
                val wv = if (wi.empty) VariantV(wi.name, Nil) else ConstructorV(wi.name)
                storePrev + (lookupEnv(wi.name, nenv) -> wv)
              }
          }
          stoPrev ++ stoi
        }
        interp(b, nenv, nsto)
      case Fun(params, b) => (CloV(params, b, env), sto)
      case Assign(x, e) =>
        val addr = lookupEnv(x, env)
        val (ev, es) = interp(e, env, sto)
        (UnitV, es + (addr -> ev))
      case App(f, args) =>
        val (fv, fs) = interp(f, env, sto)
        val (evals, stoN) = args.foldLeft((List[Value](), fs)){(acc, e) => 
          val (ev, es) = interp(e, env, acc._2)
          (acc._1 ++ List[Value](ev), es)
        }
        fv match {
          case CloV(params, b, fenv) => if (params.length != args.length) error("wrong arity") else {
            val (nenv, nsto) = (params zip evals).foldLeft((fenv, stoN)){(acc, elem) =>
              val ai = malloc(acc._2, acc._1 ++ env)
              (acc._1 + (elem._1 -> ai), acc._2 + (ai -> elem._2))
            }
            interp(b, nenv, nsto)
          }
          case ConstructorV(x) => (VariantV(x, evals), stoN)
        }
      case Match(e, cases) =>
        val (ev, es) = interp(e, env, sto)
        ev match {
          case VariantV(x, vals) =>
            cases.foldLeft(None: Option[Case]){(acc, elem) => acc match {
              case Some(_) => acc
              case None => if (elem.variant == x) Some(elem) else None
            }} match {
              case None => error("no matching case found")
              case Some(c) => if (vals.length != c.names.length) error("wrong arity") else {
                val (nenv, nsto) = (c.names zip vals).foldLeft((env, es)){(acc, elem) =>
                  val ai = malloc(acc._2)
                  (acc._1 + (elem._1 -> ai), acc._2 + (ai -> elem._2))
                }
                interp(c.body, nenv, nsto)
              }
            }
          case v => error(s"$v must be a variant")
        }
    }

    def interp(expr: Expr): Value = interp(expr, Map(), Map())._1
  }
}
