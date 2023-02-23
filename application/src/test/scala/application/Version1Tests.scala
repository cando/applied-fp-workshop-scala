package application

class Version1Tests extends munit.FunSuite {

  import application.Version1._

  // TODO: implements tests

  test("turn right command") {
    val planet = Planet(Size(5,4), List())
    val rover = Rover(Position(0, 0), Orientation.North)
    val command = Command.TurnRight
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Position(0, 0), Orientation.East))
  }

  test("turn left command") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Position(0, 0), Orientation.North)
    val command = Command.TurnLeft
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Position(0, 0), Orientation.West))
  }

  test("move forward command") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Position(0, 1), Orientation.North)
    val command = Command.MoveForward
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Position(0, 2), Orientation.North))
  }

  test("move forward command, opposite orientation") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Position(0, 1), Orientation.Sud)
    val command = Command.MoveForward
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Position(0, 0), Orientation.Sud))
  }

  test("move backward command") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Position(0, 1), Orientation.North)
    val command = Command.MoveBackward
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Position(0, 0), Orientation.North))
  }

  test("move backward command, opposite orientation") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Position(0, 1), Orientation.Sud)
    val command = Command.MoveBackward
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Position(0, 2), Orientation.Sud))
  }

  test("wrap on North") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Position(0, 3), Orientation.North)
    val command = Command.MoveForward
    val result = execute(planet, rover, command)
    assertEquals(result, Rover(Position(0, 0), Orientation.North))
  }

  test("go to opposite angle") {
    val planet = Planet(Size(5, 4), List())
    val rover = Rover(Position(0, 0), Orientation.North)
    val commands = List(Command.TurnLeft, Command.MoveForward, Command.TurnRight, Command.MoveBackward)
    val result = executeAll(planet, rover, commands)
    assertEquals(result, Rover(Position(4, 3), Orientation.North))
  }
}
