package exercises.answers

import minitest._

object TypeclassScalaTests extends SimpleTestSuite {

  case class Box[A](value: A)

  trait Semigroup[A] {
    def combine(a: A, b: A): A
  }

  implicit val intSemigroup = new Semigroup[Int] {
    def combine(a: Int, b: Int): Int = a + b
  }

  object Semigroup {
    def apply[A](implicit x: Semigroup[A]): Semigroup[A] = x
  }

  implicit class SemigroupOps[A](a: A) {
    def combine(b: A)(implicit s: Semigroup[A]): A =
      s.combine(a, b)
  }

  test("implicit parameter/value") {
    def sum[A](a: Box[A], b: Box[A])(implicit s: Semigroup[A]): Box[A] =
      Box[A](s.combine(a.value, b.value))

    assertEquals(sum(Box(42), Box(100)).value, 142)
  }

  test("interface object") {
    def sum[A: Semigroup](a: Box[A], b: Box[A]): Box[A] =
      Box[A](Semigroup[A].combine(a.value, b.value))

    assertEquals(sum(Box(42), Box(100)).value, 142)
  }

  test("interface syntax") {
    def sum[A: Semigroup](a: Box[A], b: Box[A]): Box[A] =
      Box[A](a.value.combine(b.value))

    assertEquals(sum(Box(42), Box(100)).value, 142)
  }
}
