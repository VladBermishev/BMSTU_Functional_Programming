class ComplexNumber(var realPart: Float, var imaginaryPart: Float){
  def +(rhs: ComplexNumber) = new ComplexNumber(this.realPart + rhs.realPart, this.imaginaryPart + rhs.imaginaryPart)
  def *(value: Float) = new ComplexNumber(this.realPart * value, this.imaginaryPart * value)
  def <(rhs: ComplexNumber): Boolean = {
    case this.realPart == rhs.realPart => this.imaginaryPart < rhs.imaginaryPart;
    case default => this.realPart < rhs.realPart
  }
}
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }
}