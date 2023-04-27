package cs320

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._

    // type scheme
    type TypeScheme = (Type, List[VarT])

    // type environment
    type TypeEnv = Map[String, Either[(TypeScheme, Boolean), TypeDef]]

    // Copied from lecture slides the following three functions
    def mustSame(left: Type, right: Type): Type =
      if (isSame(left, right)) left
      else notype(s"$left is not equal to $right")

    def isSame(left: Type, right: Type): Boolean =
      (left, right) match {
        case (NumT, NumT) => true
        case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
          isSame(p1, p2) && isSame(r1, r2)
        case _ => false
      }

    def notype(msg: Any): Nothing = error(s"no type: $msg")

    def wellFormedCheck(t: Type, tenv: TypeEnv): Type = t match {
      case AppT(name, targs) => 
        targs.foreach(ti => wellFormedCheck(ti, tenv))
        tenv.getOrElse(name, notype(s"$name is a free identifier")) match {
          case Right(typeDef) => if (typeDef.tparams != targs.length) error("wrong arity") else t
          // case AppT(_, tparams) => if (targs.length != tparams.length) error("wrong arity") else t
          case _ => error("invalid type")
        }
      case VarT(name) => tenv.getOrElse(name, notype(s"$name is a free identifier"))
      case IntT | BooleanT | UnitT => t
      case ArrowT(ptypes, rtype) =>
        ptypes.foreach(ti => wellFormedCheck(ti, tenv))
        wellFormedCheck(rtype, tenv)
        t
    }

    def wellFormedCheck(t: RecDef, tenv: TypeEnv): RecDef = t match {
      case Lazy(_, t, e) =>
        wellFormedCheck(t, tenv)
        mustSame(t, typeCheck(e, tenv))
        t
      // TODO: add remaining cases
    }

    def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match {
      case Id(name, targs) =>
        targs.foreach(ti => wellFormedCheck(ti, tenv))
        tenv.getOrElse(name, notype(s"$name is a free identifier")) match {
          case Left((typeScheme, _)) => if (typeScheme._2.length != targs.length) error("wrong arity") else AppT(name, targs)
          // case AppT(name, tparams) => if (targs.length != tparams.length) error("wrong arity") else AppT(name, targs)
          case _ => error("invalid type")
        }
      case IntE(_) => IntT
      case BooleanE(_) => BooleanT
      case UnitE => UnitT
      case Add(l, r) | Mul(l, r) | Div(l, r) | Mod(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        IntT
      case Eq(l, r) | Lt(l, r) =>
        mustSame(typeCheck(l, tenv), IntT)
        mustSame(typeCheck(r, tenv), IntT)
        BooleanT
      case Sequence(l, r) =>
        typeCheck(l, tenv)
        typeCheck(r, tenv)
      case If(c, t, f) =>
        mustSame(typeCheck(c, tenv), BooleanT)
        mustSame(typeCheck(t, tenv), typeCheck(f, tenv))
      case Val(m, name, typ, e, b) => match typ {
        case Some(t) => 
          wellFormedCheck(t)
          val eT = typeCheck(e, tenv)
          mustSame(eT, t)
          typeCheck(b, tenv + (name -> ((eT, []), m)))
        case None => typeCheck(b, tenv + (name -> ((typeCheck(e, tenv), []), m)))
      }
      case RecBinds(defs, b) =>
        val tenvN = defs.foldLeft(tenv){(tenvPrev, di) => 
          val tenvI = di match {
            case Lazy(x, t, e) => Map(x -> ((t, []), false))
            case RecFun(x, tparams, params, rt, b) => Map(x -> ((ArrowT(params.map(_._2), rt), tparams.map(VarT(_))), false))
            case typeDef @ TypeDef(name, tparams, variants) => if (tenv.contains(name)) error(s"$name must not be in the domain of initial tenv") else
              val tvars = tparams.map(VarT(_))
              variants.foldLeft(Map(name -> typeDef)){(genvPrev, wi) =>
                if (wi.params.length == 0) genvPrev + (wi.name -> ((AppT(name, tvars), tvars), false))
                else genvPrev + (wi.name -> ((ArrowT(wi.params, AppT(name, tvars)), tvars), false))
              }
          }
          tenvPrev ++ tenvI
        }
        // TODO: di must be well formed
    }

    def typeCheck(expr: Expr): Type = typeCheck(expr, Map())
  }

  object U {
    import Untyped._

    def interp(expr: Expr): Value = UnitV
  }
}
