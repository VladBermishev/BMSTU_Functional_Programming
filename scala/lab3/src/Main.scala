class Point[T](var x: Float, var y: Float, var z: Float, var data: T){
  override def toString: String = {
    "{ x = " + x + ", y = " + y + ", z = " + z + " }"
  }
}

class Vector[T](var A: Point[T], var B: Point[T]){
  val x: Float = B.x - A.x;
  val y: Float = B.y - A.y
  val z: Float = B.z - A.z;
  def length(): Float = x*x + y*y + z*z;
  def cross[S](rhs: Vector[S]): Vector[T] = new Vector[T](
    new Point[T](0,0,0, A.data),
    new Point[T](
      this.y*rhs.z - this.z*rhs.y,
      this.z*rhs.x - this.x*rhs.z,
      this.x*rhs.y - this.y*rhs.x,
      B.data
    )
  )
}

trait MassCenterCounter[T]{
  def mass_center(a: Point[T], b: Point[T], c: Point[T]): Point[T]
}

object MassCenterCounter{
  implicit def IntegralMassCenterCounter[T](implicit integral: Integral[T]):
  MassCenterCounter[T] =
    new MassCenterCounter[T] {
      override def mass_center(a: Point[T],
                               b: Point[T],
                               c: Point[T]): Point[T] = {
        val massSum = integral.toFloat(a.data) +
          integral.toFloat(b.data) +
          integral.toFloat(c.data)
        val x = a.x * integral.toFloat(a.data) +
          b.x * integral.toFloat(b.data) +
          c.x * integral.toFloat(c.data)
        val y = a.y * integral.toFloat(a.data) +
          b.y * integral.toFloat(b.data) +
          c.y * integral.toFloat(c.data)
        val z = a.z * integral.toFloat(a.data) +
          b.z * integral.toFloat(b.data) +
          c.z * integral.toFloat(c.data)
        new Point[T](x/massSum,y/massSum,z/massSum,integral.fromInt(0))
      }
    }
}

class Triangle[T](var A:Point[T], var B:Point[T], var C:Point[T]){
  def square(): Float = {
    val lhs = new Vector[T](A, B)
    val rhs = new Vector[T](A, C)
    lhs.cross(rhs).length()
  }
  def perimeter(): Float = {
    val AA = new Vector[T](A, B)
    val AC = new Vector[T](A, C)
    val BC = new Vector[T](B, C)
    AA.length() + AC.length() + BC.length()
  }
  def mass_center()(implicit cnt: MassCenterCounter[T]): Point[T] = {
    cnt.mass_center(this.A, this.B, this.C)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val a = new Triangle[Int](new Point[Int](1,0,0,10),
                              new Point[Int](0,1,0,10),
                              new Point[Int](0,0,1,10))
    val b = new Triangle[Float](new Point[Float](1, 1, 1, 1),
                                new Point[Float](1, 1, 1, 1),
                                new Point[Float](1, 1, 1, 1))

    println(a.mass_center().toString);
    //println(b.mass_center());
  }
}