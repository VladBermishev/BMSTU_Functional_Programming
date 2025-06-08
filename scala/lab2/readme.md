%Лабораторная работа № 2 «Введение в объектно-ориентированное
программирование на языке Scala»
% 7 апреля 2023 г.
% Владислав Бермишев, ИУ9-62Б

# Цель работы
Целью данной работы является изучение базовых объектно-ориентированных возможностей 
языка Scala.

# Индивидуальный вариант
«Нечёткое» число вида a+kδ, где a и k — числа с плавающей точкой, а δ — неизвестное 
неотрицательное бесконечно малое число.

Выполнение операций сложения «нечётких» чисел «+», умножения «нечёткого» числа на 
число с плавающей точкой «*» и сравнения «нечётких» чисел «<» определяется правилами:

    (a1+k1δ)+(a2+k2δ)≡a1 + a2+(k1+k2)δ;
    c(a+kδ)≡ca+ckδ;
    a1+k1δ<a2+k2δ тогда и только тогда, когда либо a1<a2, либо (a1=a2)∧(k1<k2).


# Реализация и тестирование

```scala
import scala.language.implicitConversions

class ComplexNumber(var realPart: Float, var imaginaryPart: Float){
  def +(rhs: ComplexNumber) = new ComplexNumber(
    this.realPart + rhs.realPart, this.imaginaryPart + rhs.imaginaryPart
  )
  def *(rhs: ComplexNumber) = new ComplexNumber(
    this.realPart * rhs.realPart - this.imaginaryPart * rhs.imaginaryPart,
    this.imaginaryPart * rhs.realPart + this.realPart * rhs.imaginaryPart
  )
  def *(value: Float) = new ComplexNumber(
    this.realPart * value,
    this.imaginaryPart * value
  )
  def <(rhs: ComplexNumber) =
    if (this.realPart == rhs.realPart) this.imaginaryPart < rhs.imaginaryPart
    else
      this.realPart < rhs.realPart

  override def toString: String = this.realPart + " + " + this.imaginaryPart + "i"
}
implicit def floatToComplex(float: Float): ComplexNumber = new ComplexNumber(
  float,
  0
)
implicit def intToComplex(int: Int): ComplexNumber = floatToComplex(int.toFloat)

val a = new ComplexNumber(1,1)
val b = new ComplexNumber(1, 2)
a + b
a * 3
3 * a
a < b
```

# Вывод
изучил базовые объектно-ориентированные возможности языка Scala.