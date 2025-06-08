sealed trait Expr {
  def HKRelation(expr: Expr): Boolean
}

case class Variable(name: String) extends Expr {
  override def toString: String = {
    name.toLowerCase()
  }

  override def HKRelation(expr: Expr): Boolean = expr match {
    case Variable(vname) => true
    case Function(fname, fargs) => fargs.nonEmpty && fargs.exists(p => this.HKRelation(p))
    case default => false
  }
}

case class Constant(name: String) extends Expr {
  override def toString: String = {
    name.toUpperCase()
  }

  override def HKRelation(expr: Expr): Boolean = expr match {
    case Constant(cname) => name == cname
    case Function(fname, fargs) => fargs.nonEmpty && fargs.exists(p => this.HKRelation(p))
    case default => false
  }
}

case class Function(name: String, args: List[Expr] = List()) extends Expr {
  override def toString: String = {
    val args_str =
      if (args.isEmpty)
        "()"
      else if (args.length == 1)
        "(" + args.head.toString + ")"
      else {
        var buffer = args.head.toString
        for (idx <- 1 until args.length) {
          buffer += ", " + args.apply(idx).toString
        }
        "(" + buffer + ")"
      }
    name + args_str
  }

  override def HKRelation(expr: Expr): Boolean = expr match {
    case Function(fname, fargs) =>
      fargs.nonEmpty && fargs.exists(p => this.HKRelation(p)) ||
        (name == fname && args.length == fargs.length &&
          args.iterator.zip(fargs.iterator).forall(x => x._1.HKRelation(x._2)))
    case default => false
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val test1 = (Function("Tree", Function("Leaf", Constant("A") :: Nil) ::
      Function("Leaf", Variable("x") :: Nil) :: Nil),
      Function("Tree", Function("Tree", Function("Leaf", Constant("A") :: Nil) ::
        Function("Leaf", Function("f", Variable("y") :: Variable("z") :: Nil) :: Nil) :: Nil) ::
        Function("g", Variable("y") :: Variable("z") :: Nil) :: Nil))
    val test2 = (Function("f", Function("g", Function("h", Variable("x") :: Nil) :: Nil) :: Nil),
      Function("f", Function("h", Variable("x") :: Nil) :: Nil))
    val tests = test1 :: test2 :: Nil
    var testCnt = 1
    for (test <- tests) {
      println("Test #" + testCnt)
      println(test._1.toString + " <= " + test._2.toString + " -> " + test._1.HKRelation(test._2))
      testCnt += 1
    }
  }
}