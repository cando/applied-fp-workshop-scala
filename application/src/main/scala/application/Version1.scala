package application

import cats.instances.int

/*
    ## V1 - Focus on the center (pure domain logic)

    Develop an API (types and functions) that executes commands:

    - Implement all commands logic.
    - Commands are sent in batch and executed sequentially.
    - The planet grid has a wrapping effect from one edge to another (pacman).
    - For now, ignore obstacle detection logic
 */
object Version1 {

  // TODO 1: Those type alias are only placeholders,
  //  use correct type definitions and feel free to add more...

  sealed case class Position(x: Int, y: Int)

  // type Obstacle = Position
  sealed case class Obstacle(position: Position)

  sealed case class Size(width: Int, height: Int)

  enum Orientation {
    case North
    case Sud
    case West
    case East
  }

  case class Rover(position: Position, orientation: Orientation)

  case class Planet(size: Size, obstacles: List[Obstacle])

  enum Command {
    case TurnLeft
    case TurnRight
    case MoveForward//(steps: Int)
    case MoveBackward//(steps: Int)
  }

  // TODO 2: Execute all commands and accumulate final rover state
  def executeAll(planet: Planet, rover: Rover, commands: List[Command]): Rover =
    commands.foldLeft(rover)((r, cmd) => execute(planet, r, cmd))

  // TODO 3: Dispatch one command to a specific function
  def execute(planet: Planet, rover: Rover, command: Command): Rover =
    command match {
      case Command.TurnLeft => turnLeft(rover)
      case Command.TurnRight => turnRight(rover)
      case Command.MoveForward => moveForward(planet, rover)
      case Command.MoveBackward => moveBackward(planet, rover)
    }

  // TODO 4: Change rover orientation
  def turnRight(rover: Rover): Rover = Rover(rover.position, rover.orientation match {
    case Orientation.North => Orientation.East
    case Orientation.Sud => Orientation.West
    case Orientation.West => Orientation.North
    case Orientation.East => Orientation.Sud
  })

  // TODO 5: Change rover orientation
  def turnLeft(rover: Rover): Rover = Rover(rover.position, rover.orientation match {
    case Orientation.North => Orientation.West
    case Orientation.Sud => Orientation.East
    case Orientation.West => Orientation.Sud
    case Orientation.East => Orientation.North
  })

  // TODO 6: Change rover position
  def moveForward(planet: Planet, rover: Rover): Rover = Rover(rover.orientation match {
    case Orientation.North => rover.position.copy(y = wrap(rover.position.y, planet.size.height,1))
    case Orientation.Sud => rover.position.copy(y = wrap(rover.position.y, planet.size.height,-1))
    case Orientation.West => rover.position.copy(x = wrap(rover.position.x, planet.size.width,-1))
    case Orientation.East => rover.position.copy(x = wrap(rover.position.x, planet.size.width,1))
  }, rover.orientation)

  // TODO 7: Change rover position
  def moveBackward(planet: Planet, rover: Rover): Rover = Rover(rover.orientation match {
    case Orientation.North => rover.position.copy(y = wrap(rover.position.y, planet.size.height, -1))
    case Orientation.Sud => rover.position.copy(y = wrap(rover.position.y, planet.size.height, 1))
    case Orientation.West => rover.position.copy(x = wrap(rover.position.x, planet.size.width, 1))
    case Orientation.East => rover.position.copy(x = wrap(rover.position.x, planet.size.width, -1))
  }, rover.orientation)

  // NOTE: utility function to get the pacman effect
  def wrap(value: Int, limit: Int, delta: Int): Int =
    (((value + delta) % limit) + limit) % limit
}
