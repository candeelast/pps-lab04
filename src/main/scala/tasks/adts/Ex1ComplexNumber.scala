package tasks.adts

import scala.math.abs

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    case class ComplexNum(re: Double, im: Double)
    type Complex = ComplexNum
    def complex(re: Double, im: Double): Complex = ComplexNum(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = ComplexNum(complex.re + other.re, complex.im + other.im)
      def subtract(other: Complex): Complex = ComplexNum(complex.re - other.re, complex.im - other.im)
      def asString(): String =
        (complex.re, complex.im) match
          case (0, 0) => "0.0"
          case (_, 0) => complex.re.toString
          case (0, _) => complex.im.toString + "i"
          case (_, _) => complex.im match
            case n if n > 0 => complex.re.toString + " + " + complex.im.toString + "i"
            case _ => complex.re.toString + " - " + abs(complex.im).toString + "i"

