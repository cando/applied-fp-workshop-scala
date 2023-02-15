package exercises

/*
 * Our most used Scala features are:
 * - Case class
 * - Companion Object
 * - Apply function
 * - Pattern match
 * - Trait as interface
 * - Trait as mixin
 */

case class Person(name: String, age: Int) {
  def apply(data: String): String =
    s"$data mi chiamo ${name}!"

  def makeOlder(incr: Int): Person =
    // Person(name, age + incr)
    copy(age = age + incr)
}

object Person {
  def create(data: String): Person = {
    val split = data.split(";")
    Person(split(0), split(1).toInt)
  }

  def apply(info: String): Person =
    create(info)

  def isFake(person: Person): Boolean =
    person match {
      case Person("foo", _)          => true
      case Person("bar", _)          => true
      case Person(_, age) if age < 0 => true
      case _                         => false

    }
}

trait Fruit {
  def stringify: String

  def eatenBy(who: String): String = s"$who ate $stringify"
}
class Apple extends Fruit {

  override def stringify: String = "an apple"

}
class Banana extends Fruit {

  override def stringify: String = "a banana"

}

class ScalaRecap extends munit.FunSuite {

  test("define case class") {
    val result = Person("foo", 56)
    assertEquals(result, Person("foo", 56))
  }

  test("define the case class's companion object") {
    val result = Person.create("foo;56")
    assertEquals(result, Person("foo", 56))
  }

  test("case class apply") {
    val result = Person("foo", 56)("Ciao,")
    assertEquals(result, "Ciao, mi chiamo foo!")
  }

  test("companion object apply") {
    val result = Person("foo;56")("Ciao,")
    assertEquals(result, "Ciao, mi chiamo foo!")
  }

  test("update case class state") {
    val p = Person("foo", 56)
    val result = p.makeOlder(100)
    assertEquals(result.age, 156)
  }
  test("pattern match") {
    import Person._
    assert(isFake(Person("foo", 10)))
    assert(isFake(Person("bar", 10)))
    assert(isFake(Person("baz", -10)))
    assert(!isFake(Person("baz", 10)))
  }

  test("trait as interface (part 1)") {
    assert(Apple().isInstanceOf[Fruit])
    assert(Banana().isInstanceOf[Fruit])
  }

  test("trait as interface (part 2)") {
    assertEquals(Apple().stringify, "an apple")
    assertEquals(Banana().stringify, "a banana")
  }

  test("trait as mixin") {
    assertEquals(Apple().eatenBy("foo"), "foo ate an apple")
    assertEquals(Banana().eatenBy("bar"), "bar ate a banana")
  }
}
