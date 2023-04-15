package airplaneGame


trait Action(plane: Airplane):
  
  def execute(): Unit


class GoingToRunway(plane: Airplane, runway: Runway) extends Action(plane):

  val target: Coord = runway.start.toCoord(plane.game.coordPerTile)

  def onRunway: Boolean =
    plane.location == target

  def execute() = ???


class CirclingLeft(plane: Airplane) extends Action(plane):

  def execute() =
    plane.bearing -= plane.maxTurn
    plane.fuel -= 0.2


class CirclingRight(plane: Airplane) extends Action(plane):

  def execute() =
    plane.bearing += plane.maxTurn
    plane.fuel -= 0.2


class Leaving(plane: Airplane) extends Action(plane):

  def execute() =
    plane.cruiseSpeed()
    plane.fuel -= 0.2



class Landing(plane: Airplane, runway: Runway) extends Action(plane):

  val neededSpeed = 1
  val originalSpeed = plane.speed

  def hasLanded: Boolean =
    plane.speed == neededSpeed && plane.location == runway.end.toCoord(plane.game.coordPerTile)

  def hasCrashed: Boolean =
    plane.location == runway.end.toCoord(plane.game.coordPerTile) && !hasLanded

  def execute() =
    if hasCrashed then
      plane.crashed = true
    else if hasLanded then
      plane.action = TaxiingToGate(plane)
    if plane.speed > neededSpeed then
      plane.speed = math.max(1, plane.speed - originalSpeed / (plane.neededRunway * plane.game.coordPerTile))


class TakingOff(plane: Airplane, runway: Runway) extends Action(plane):

  val neededSpeed: Double = 5

  def isOff: Boolean =
    plane.speed > neededSpeed && plane.location == runway.end.toCoord(plane.game.coordPerTile)

  def hasCrashed: Boolean =
    plane.location == runway.end.toCoord(plane.game.coordPerTile) && !isOff

  def execute() =
    plane.fuel -= 0.2
    if hasCrashed then
      plane.crashed = true
    else if isOff then
      Leaving(plane)
    if plane.speed < neededSpeed then
      plane.speed += neededSpeed / (plane.neededRunway * plane.game.coordPerTile)



class Crashed(plane: Airplane) extends Action(plane):

  def execute() =
    if plane.speed != 0 then
      plane.speed = math.max(0, plane.speed - 1)



class TaxiingToGate(plane: Airplane) extends Action(plane):

  val timeToTaxi = 150
  var timer = 0
  var taxiing = false

  def execute() =
    if !taxiing && !plane.game.grid.gates.forall( _.plane.isDefined ) then
      plane.game.grid.gates.filter( _.plane.isEmpty ).head.plane = Some(plane)
    if timer == timeToTaxi then
      plane.action = Boarding(plane)
    else if taxiing then
      timer += 1


class Boarding(plane: Airplane) extends Action(plane):

  val timeToBoard = 150
  var timer = 0

  var hasBoarded = false

  def execute() =
    if timer == timeToBoard then
      hasBoarded = true
      plane.fuel = plane.maxFuel
    else
      timer += 1


class TaxiingToRunway(plane: Airplane, runway: Runway) extends Action(plane):

  val timeToTaxi = 150
  var timer = 0
  var onRunway = false

  def execute() =
    if timer == timeToTaxi then
      onRunway = true
    else
      timer += 1
