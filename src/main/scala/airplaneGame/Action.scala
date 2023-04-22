package airplaneGame

import scala.util.Random


trait Action(plane: Airplane):
  
  def execute(): Unit


class GoingToRunway(plane: Airplane, runway: Runway) extends Action(plane):

  val target: Coord = runway.start.toCoord(plane.game.coordPerTile)

  def onRunway: Boolean =
    plane.location == target

  def execute() =
    //println(s"Plane ${plane.id}, at ${plane.location}")
    //plane.game.grid.runways.foreach( n => println(n.start.toCoord(plane.game.coordPerTile)) )
    //println("Target:" + target)
    plane.fuel -= 0.2
    if math.abs(plane.location.x - target.x) < 15 && math.abs(plane.location.y - target.y) < 15 then
      plane.location = target
      plane.action = Landing(plane, runway)
      plane.bearing = Degrees(runway.direction.bearing)
      return
    val bearingToTargetDiff: Int = plane.location.bearingTo(target) - plane.bearing.value
    val toTurn = (plane.maxTurn * (bearingToTargetDiff.toDouble / 180)).ceil.toInt
    if bearingToTargetDiff > 0 && bearingToTargetDiff < 180 then
      plane.bearing += toTurn
    else
    if bearingToTargetDiff <= 180 && plane.location.bearingTo(target) < plane.bearing.value then
      plane.bearing -= toTurn
    else if bearingToTargetDiff <= 180 && plane.location.bearingTo(target) > plane.bearing.value then
      plane.bearing += toTurn
    else if bearingToTargetDiff > 180 && plane.location.bearingTo(target) > plane.bearing.value then
      plane.bearing -= toTurn
    else if bearingToTargetDiff > 180 && plane.location.bearingTo(target) < plane.bearing.value then
      plane.bearing += toTurn


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

  def atEndOfRunway = 15 > math.abs(plane.location.x - runway.end.toCoord(plane.game.coordPerTile).x + (plane.location.y - runway.end.toCoord(plane.game.coordPerTile).y))

  def execute() =
    if plane.speed == neededSpeed && atEndOfRunway then
      plane.action = TaxiingToGate(plane)
      plane.speed = 0
    else if atEndOfRunway then
      plane.action = Crashed(plane)
    if plane.speed > neededSpeed then
      plane.speed = math.max(1, plane.speed - ((4.0 * 4 / (2 * plane.neededRunway * plane.game.coordPerTile))))


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

  var crashedFor = 0

  def execute() =
    if crashedFor == 0 then
      plane.game.airplanesOnMap.remove(plane.game.airplanesOnMap.zipWithIndex.filter( _._1.id == plane.id ).head._2)
      plane.game.crashedPlanes.enqueue(plane)
    crashedFor += 1
    if plane.speed != 0 then
      plane.speed = math.max(0, plane.speed - 0.2)
    if crashedFor > 40 then
      plane.game.crashedPlanes.dequeue()
      println(plane.game.crashedPlanes.length)



class TaxiingToGate(plane: Airplane) extends Action(plane):

  val timeToTaxi = 150
  var timer = 0
  var taxiing = false
  var gate: Option[Gate] = None

  def initTaxi() =
    plane.speed = 0
    plane.location = Coord(-100, -100)

  def execute() =
    if !taxiing && !plane.game.grid.gates.forall( _.plane.isDefined ) then
      val gateToAssign = plane.game.grid.gates.filter( _.plane.isEmpty ).head
      gateToAssign.plane = Some(plane)
      gate = Some(gateToAssign)
      initTaxi()
      taxiing = true
    if timer == timeToTaxi then
      plane.action = Boarding(plane)
      plane.location = gate.get.loc.toCoord(plane.game.coordPerTile)
      plane.bearing = Degrees(0)
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


class Arriving(plane: Airplane) extends Action(plane):

  var toArrive = 15

  def execute() =
    //println(s"Plane ${plane.id}, arriving in $toArrive")
    if toArrive == 0 then
      plane.action = GoingToRunway(plane, plane.game.grid.runways(Random.nextInt(plane.game.grid.numberOfRunways)))
      plane.location = Coord(Random.nextInt(plane.game.width * plane.game.coordPerTile), 0) //TODO fix
      val indexInArriving =
        plane.game.airplanesToArrive.zipWithIndex.filter( _._1.id == plane.id ).head._2
      plane.game.airplanesToArrive.remove(indexInArriving)
      plane.game.airplanesOnMap.append(plane)
    else
      toArrive -= 1