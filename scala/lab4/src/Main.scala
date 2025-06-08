sealed trait Expr

case class Varname(name: String) extends Expr {
  override def toString: String = {
    name
  }
}

case class Sum(lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = {
    val lhs_str = lhs match {
      case Varname(vname) => lhs.toString
      case Sum(slhs, srhs) => "(" + lhs.toString + ")"
      case Mult(mlhs, mrhs) => "(" + lhs.toString + ")"
      case Let(name, llhs, lrhs) => "(" + lhs.toString + ")"
    }
    val rhs_str = rhs match {
      case Varname(vname) => rhs.toString
      case Sum(slhs, srhs) => "(" + rhs.toString + ")"
      case Mult(mlhs, mrhs) => "(" + rhs.toString + ")"
      case Let(name, llhs, lrhs) => "(" + rhs.toString + ")"
    }
    lhs_str + " + " + rhs_str
  }
}

case class Mult(lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = {
    val lhs_str = lhs match {
      case Varname(vname) => lhs.toString
      case Sum(slhs, srhs) => "(" + lhs.toString + ")"
      case Mult(mlhs, mrhs) => "(" + lhs.toString + ")"
      case Let(name, llhs, lrhs) => "(" + lhs.toString + ")"
    }
    val rhs_str = rhs match {
      case Varname(vname) => rhs.toString
      case Sum(slhs, srhs) => "(" + rhs.toString + ")"
      case Mult(mlhs, mrhs) => "(" + rhs.toString + ")"
      case Let(name, llhs, lrhs) => "(" + rhs.toString + ")"
    }
    lhs_str + " * " + rhs_str
  }
}

case class Let(variable: Varname, lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = {
    val lhs_str = lhs match {
      case Varname(vname) => lhs.toString
      case Sum(slhs, srhs) => "(" + lhs.toString + ")"
      case Mult(mlhs, mrhs) => "(" + lhs.toString + ")"
      case Let(name, llhs, lrhs) => "(" + lhs.toString + ")"
    }
    val rhs_str = rhs match {
      case Varname(vname) => rhs.toString
      case Sum(slhs, srhs) => "(" + rhs.toString + ")"
      case Mult(mlhs, mrhs) => "(" + rhs.toString + ")"
      case Let(name, llhs, lrhs) => "(" + rhs.toString + ")"
    }
    "let " + variable.name + " = " + lhs_str + " in " + rhs_str
  }
}

object Main {
  var cnt = 0

  def generateVarname(): Varname = {
    cnt += 1
    Varname("v" + cnt.toString)
  }

  def countExpr(expr: Expr, expr_to_find: Expr): Int = {
    var res = 0
    if (expr == expr_to_find) res = 1 else res = 0
    expr match {
      case Varname(name) => res
      case Sum(lhs, rhs) => res + countExpr(lhs, expr_to_find) + countExpr(rhs, expr_to_find)
      case Mult(lhs, rhs) => res + countExpr(lhs, expr_to_find) + countExpr(rhs, expr_to_find)
      case Let(name, lhs, rhs) => res +
        countExpr(lhs, expr_to_find) +
        countExpr(rhs, expr_to_find)
    }
  }

  def replaceByExpr(expr: Expr, to_replace: Expr, replacement: Expr): Expr = {
    if (expr == to_replace)
      replacement
    else {
      expr match {
        case Varname(name) => expr
        case Sum(lhs, rhs) => Sum(replaceByExpr(lhs, to_replace, replacement),
          replaceByExpr(rhs, to_replace, replacement))
        case Mult(lhs, rhs) => Mult(replaceByExpr(lhs, to_replace, replacement),
          replaceByExpr(rhs, to_replace, replacement))
        case Let(lvarname, lhs, rhs) => Let(lvarname,
          replaceByExpr(lhs, to_replace, replacement),
          replaceByExpr(rhs, to_replace, replacement))
      }
    }
  }

  def commonPart(lhs: Expr, rhs: Expr): Expr = {
    if (!lhs.isInstanceOf[Varname] && countExpr(rhs, lhs) > 0) {
      lhs
    } else {
      lhs match {
        case Varname(name) => null
        case Sum(slhs, srhs) => {
          var common_part = commonPart(slhs, rhs)
          if (common_part != null) common_part
          else {
            common_part = commonPart(srhs, rhs)
            if (common_part != null) common_part else null
          }
        }
        case Mult(mlhs, mrhs) => {
          var common_part = commonPart(mlhs, rhs)
          if (common_part != null) common_part
          else {
            common_part = commonPart(mrhs, rhs)
            if (common_part != null) common_part else null
          }
        }
        case Let(name, llhs, lrhs) => {
          var common_part = commonPart(llhs, rhs)
          if (common_part != null) common_part
          else {
            common_part = commonPart(lrhs, rhs)
            if (common_part != null) common_part else null
          }
        }
      }
    }
  }

  def letsOptimize(expr: Expr): Expr = expr match {
    case Sum(lhs, rhs) => {
      val new_lhs = letsOptimize(lhs)
      val new_rhs = letsOptimize(rhs)
      val common_part: Expr = commonPart(lhs, rhs)
      if (common_part != null && !common_part.isInstanceOf[Varname]) {
        val new_varname = generateVarname()
        letsOptimize(Let(new_varname,
          common_part,
          Sum(replaceByExpr(new_lhs, common_part, new_varname),
              replaceByExpr(new_rhs, common_part, new_varname))))
      } else {
        Sum(new_lhs, new_rhs)
      }
    }
    case Mult(lhs, rhs) => {
      val new_lhs = letsOptimize(lhs)
      val new_rhs = letsOptimize(rhs)
      val common_part: Expr = commonPart(lhs, rhs)
      if (common_part != null && !common_part.isInstanceOf[Varname]) {
        val new_varname = generateVarname()
        letsOptimize(Let(new_varname,
          common_part,
          Mult(replaceByExpr(new_lhs, common_part, new_varname),
               replaceByExpr(new_rhs, common_part, new_varname))))
      } else {
        Mult(new_lhs, new_rhs)
      }
    }
    case Let(varname, lhs, rhs) => {
      val new_lhs = letsOptimize(lhs)
      val new_rhs = letsOptimize(rhs)
      if (countExpr(rhs, varname) == 1) replaceByExpr(rhs, varname, lhs)
      else Let(varname, new_lhs, new_rhs)
    }
    case Varname(name) => expr
  }

  def main(args: Array[String]): Unit = {
    val test1 = Mult(Sum(Varname("x"), Varname("y")), Sum(Varname("x"), Varname("y")))
    val test2 = Let(Varname("x"),
      Sum(Varname("y"), Varname("z")),
      Mult(Varname("a"), Varname("x")))
    val test3 = Sum(Mult(Sum(Varname("a"), Varname("b")),
      Sum(Varname("c"), Varname("d"))),
      Sum(Sum(Varname("a"), Varname("b")),
          Sum(Varname("c"), Varname("d"))))
    val tests = test1 :: test2 :: test3 :: Nil
    var testCnt = 1
    for (test <- tests) {
      println("Test #" + testCnt)
      println("Before optimization:")
      println(test.toString)
      println("After optimization:")
      println(letsOptimize(test).toString)
      testCnt += 1
    }
  }
}