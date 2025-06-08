import scala.language.implicitConversions

class ComplexNumber(var realPart: Float, var imaginaryPart: Float){
  def +(rhs: ComplexNumber) = new ComplexNumber(this.realPart + rhs.realPart, this.imaginaryPart + rhs.imaginaryPart)
  def *(rhs: ComplexNumber) = new ComplexNumber(this.realPart * rhs.realPart,
    this.imaginaryPart * rhs.realPart + this.realPart * rhs.imaginaryPart)
  def *(value: Float) = new ComplexNumber(this.realPart * value, this.imaginaryPart * value)
  def <(rhs: ComplexNumber) =
    if (this.realPart == rhs.realPart) this.imaginaryPart < rhs.imaginaryPart else
    this.realPart < rhs.realPart

  override def toString: String = this.realPart + " + " + this.imaginaryPart + "i"
}
implicit def floatToComplex(float: Float): ComplexNumber = new ComplexNumber(float, 0)
implicit def intToComplex(int: Int): ComplexNumber = floatToComplex(int.toFloat)

val a = new ComplexNumber(1,1)
val b = new ComplexNumber(1, 2)
a + b
a * 3
3 * a
a < b