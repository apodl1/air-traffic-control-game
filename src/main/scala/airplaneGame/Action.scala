package airplaneGame

import scala.util.Random
import airplaneGame.CompassDir



trait Action(plane: Airplane):
  val coordPerTile = plane.game.coordPerTile
  val runwayActions: Vector[String] = Vector("Landing", "Expediting", "TakingOff")
  var boarded = false

  def overlappingRunwayPlaneWith(thisPlane: Airplane) =
    (plane.game.airplanesOnMap ++ plane.game.crashedPlanes)
      .filter( n => n != thisPlane && runwayActions.exists( m => n.action.getClass.getTypeName.contains(m) ) )
      .filter( n => 5 > math.abs(plane.location.x - n.location.x + (plane.location.y - n.location.y)) )

  def execute(): Unit


class Arriving(plane: Airplane) extends Action(plane): //puts plane on map
  private val toArrive = 15
  private var timer = 0

  def execute() =
    //println(s"Plane ${plane.id}, arriving in $toArrive")
    timer += 1
    if toArrive == timer then
      plane.game.newArrivalMessage("Plane #" + plane.id + " requesting permission to land\n")
      plane.action = GoingToRunway(plane, plane.game.grid.runways(Random.nextInt(plane.game.grid.numberOfRunways)))
      plane.location = Coord(Random.nextInt(plane.game.width * coordPerTile), 0) //TODO fix
      val indexInArriving =
        plane.game.airplanesToArrive.zipWithIndex.filter( _._1.id == plane.id ).head._2
      plane.game.airplanesToArrive.remove(indexInArriving)
      plane.game.airplanesOnMap.append(plane)
    if toArrive - timer == 5 then
      plane.game.newArrivalMessage("Plane #" + plane.id + " arriving in 5\n")



class GoingToRunway(plane: Airplane, runway: Runway) extends Action(plane):
  private val runwayStart: Coord = runway.start.toCoord(coordPerTile)
  private var oriented = false
  private var clearedForEntry = false
  private val entryPoint: Coord = (runway.start - runway.direction - runway.direction).toCoord(coordPerTile)

  def orientationPoint: Coord =
    if runway.isHorizontal then
      if plane.location.y < runwayStart.y * coordPerTile then
        (entryPoint.toGridPos(coordPerTile) - runway.direction - runway.direction - GridPos(0, 2)).toCoord(coordPerTile)
      else
        (entryPoint.toGridPos(coordPerTile) - runway.direction - runway.direction + GridPos(0, 2)).toCoord(coordPerTile)
    else
      if plane.location.x < runwayStart.x * coordPerTile then
        (entryPoint.toGridPos(coordPerTile) - runway.direction - runway.direction - GridPos(2, 0)).toCoord(coordPerTile)
      else
        (entryPoint.toGridPos(coordPerTile) - runway.direction - runway.direction + GridPos(2, 0)).toCoord(coordPerTile)

  def conditionToOrient: Boolean =
    plane.location.toGridPos(coordPerTile).isSameAs(orientationPoint.toGridPos(coordPerTile))

  def conditionToEntry: Boolean =
    math.abs(plane.location.x - entryPoint.x) < 15 && math.abs(plane.location.y - entryPoint.y) < 15

  def bearingToDiff(target: Coord) = math.abs(plane.location.bearingTo(target) - plane.bearing.value)

  def onCoord(coord: Coord): Boolean = math.abs(plane.location.x - coord.x) < 15 && math.abs(plane.location.y - coord.y) < 15

  override def toString: String = "Going to Runway #" + runway.index

  def goToPoint(target: Coord) =
    val bearingToTarget = plane.location.bearingTo(target)
    val bearingToTargetDiff: Int = bearingToDiff(target)
    //how much to turn algo:
    val toTurn =
      if bearingToTargetDiff <= 180 then
        (plane.maxTurn * (bearingToTargetDiff.toDouble / 180)).ceil.toInt
      else
        (plane.maxTurn * ((360 - bearingToTargetDiff.toDouble) / 180)).ceil.toInt
    //where to turn algo:
    if plane.bearing.value != bearingToTarget then
      if plane.location.x < target.x && ((plane.bearing.value < bearingToTarget) || (360 - (180 - bearingToTarget) < plane.bearing.value)) then
        plane.bearing += toTurn
      else if plane.location.x > target.x && (plane.bearing.value < bearingToTarget && plane.bearing.value > 180 - bearingToTarget) then
        plane.bearing += toTurn
      else
        plane.bearing -= toTurn

  def execute() =
    plane.fuel -= 0.2
    //if on target:
    if onCoord(runwayStart) && bearingToDiff(runwayStart) < 30 && clearedForEntry then
      if runway.isHorizontal then
        plane.location = Coord(plane.location.x, runwayStart.y)
      else
        plane.location = Coord(runwayStart.x, plane.location.y)
      plane.action = Landing(plane, runway)
      plane.bearing = Degrees(runway.direction.bearing)
    else if onCoord(runwayStart) && clearedForEntry then
      println("reseted")
      clearedForEntry = false
      oriented = false
    else if clearedForEntry then
      goToPoint(runwayStart)
    else if conditionToEntry && oriented then
      println("cleared for entry")
      clearedForEntry = true
      goToPoint(runwayStart)
    else if oriented then
      goToPoint(entryPoint)
    else if conditionToOrient then
      oriented = true
      goToPoint(entryPoint)
    else
      goToPoint(orientationPoint)


class CirclingLeft(plane: Airplane) extends Action(plane):
  override def toString: String = "Circling left. Remaining fuel: " + plane.fuelToDisplay

  def execute() =
    plane.bearing -= plane.maxTurn
    plane.fuel -= 0.2


class CirclingRight(plane: Airplane) extends Action(plane):
  override def toString: String = "Circling right. Remaining fuel: " + plane.fuelToDisplay

  def execute() =
    plane.bearing += plane.maxTurn
    plane.fuel -= 0.2


class Landing(plane: Airplane, runway: Runway) extends Action(plane):
  private val neededSpeed = 1
  private val originalSpeed = plane.speed

  override def toString: String = "Landing at Runway #" + runway.index

  def atEndOfRunway = 10 > math.abs(plane.location.x - runway.end.toCoord(coordPerTile).x + (plane.location.y - runway.end.toCoord(coordPerTile).y))

  def execute() =
    if overlappingRunwayPlaneWith(plane).nonEmpty then //if overlapping plane
      plane.action = Crashed(plane)
      overlappingRunwayPlaneWith(plane).foreach(n => n.action = Crashed(n) )
    if plane.speed == neededSpeed && atEndOfRunway then //landed
      var whereDraw = runway.arrivingWaitArea
      runway.airplanesWaitingForGate.foreach( n => whereDraw = whereDraw - runway.direction )
      plane.location = whereDraw.toCoord(coordPerTile)
      plane.action = TaxiingToGate(plane)
      plane.speed = 0
    else if atEndOfRunway then //too fast at end
      plane.action = Crashed(plane)
    if plane.speed > neededSpeed then
      plane.speed = math.max(1, plane.speed - ((4.0 * 4 / (2 * plane.neededRunway * coordPerTile))))

class Expediting(plane: Airplane) extends Action(plane):
  private var beenOffFor = 0
  private var isOff = false

  def execute(): Unit =
    if overlappingRunwayPlaneWith(plane).nonEmpty then //if overlapping plane
      plane.action = Crashed(plane)
      overlappingRunwayPlaneWith(plane).foreach(n => n.action = Crashed(n) )
    if plane.speed > 3 then
      isOff = true
    if isOff then
      beenOffFor += 1
    if beenOffFor > 40 then
      plane.action = CirclingLeft(plane)
    if !isOff then
      plane.speed += 0.2


class Crashed(plane: Airplane) extends Action(plane):
  private var crashedFor = 0

  def execute() =
    if crashedFor == 0 then
      if plane.game.airplanesOnMap.lift(plane.game.airplanesOnMap.zipWithIndex.filter( _._1.id == plane.id ).head._2).isDefined && plane.game.airplanesOnMap.zipWithIndex.exists(_._1.id == plane.id) then
        plane.game.airplanesOnMap.remove(plane.game.airplanesOnMap.zipWithIndex.filter( _._1.id == plane.id ).head._2)
      plane.game.crashedPlanes.enqueue(plane)
    crashedFor += 1
    if plane.speed != 0 then
      plane.speed = math.max(0, plane.speed - 0.2)
    if crashedFor > 40 then
      plane.game.crashedPlanes.dequeue()



class TaxiingToGate(plane: Airplane) extends Action(plane):
  private val timeToTaxi = 70
  private var timer = 0
  private var taxiing = false
  private var gate: Option[Gate] = None

  private var description = "Waiting for free gate"

  override def toString: String = description

  def initTaxi() =
    plane.speed = 0
    plane.location = Coord(-100, -100)

  def execute() =
    if !taxiing && !plane.game.grid.gates.forall( _.plane.isDefined ) then //start taxiing
      val gateToAssign = plane.game.grid.gates.filter( _.plane.isEmpty ).head
      gateToAssign.plane = Some(plane)
      gate = Some(gateToAssign)
      initTaxi()
      taxiing = true
      description = "Taxiing to gate"
    if timer == timeToTaxi then //end taxiing
      plane.action = Boarding(plane)
      plane.location = gate.get.loc.toCoord(coordPerTile) + Coord(0, 15)
      plane.bearing = Degrees(0)
    else if taxiing then
      timer += 1
      description = "Taxiing to gate, time left: " + (timeToTaxi - timer)


class Boarding(plane: Airplane) extends Action(plane):
  private val timeToBoard = 100
  private var timer = 0
  private val tenPercent = timeToBoard / 10
  def percentiles = timer / tenPercent

  private val sharps = "|##########|"
  private val dots = "__________|"
  private val start = "Boarding, progress: "
  private var description = "Boarding, time to board: " + (timeToBoard - timer) + "\nProgress: " + sharps.head + dots

  override def toString: String = description

  def execute() =
    if timer == timeToBoard then
      plane.fuel = plane.maxFuel
      plane.game.newScore(plane.passengers + " passengers arrived.", 50)
      plane.action = Boarded(plane)
    else
      timer += 1
    if timer <= timeToBoard && timer % tenPercent == 0 then
      description = start + sharps.take(percentiles + 1) + dots.drop(percentiles)

class Boarded(plane: Airplane) extends Action(plane):
  private var timer = 0
  private val description = "Boarded, awaiting order to taxi"
  override def toString: String = description

  def execute() =
    boarded = true
    timer += 1



class TaxiingToRunway(plane: Airplane, runway: Runway) extends Action(plane):
  private val timeToTaxi = 70
  private var timer = 0

  override def toString: String = "Taxiing to runway #" + runway.index + ". Time left: " + (timeToTaxi - timer)

  def execute() =
    plane.game.grid.gates.filter( _.plane == Some(plane) ).foreach( _.plane = None )
    plane.location = Coord(-100, -100)
    if timer == timeToTaxi then
      var whereToDraw = runway.departingWaitArea
      runway.airplanesWaitingForTakeoff.foreach(n => whereToDraw = whereToDraw + runway.direction )
      plane.location = whereToDraw.toCoord(coordPerTile)
      plane.action = WaitingOnRunway(plane, runway)
    else
      timer += 1

class WaitingOnRunway(plane: Airplane, runway: Runway) extends Action(plane):
  private var timer = 0

  override def toString: String = "Waiting for permission to take off"

  def execute(): Unit =
    if timer == 0 then
      runway.airplanesWaitingForTakeoff.append(plane)
    timer += 1

class TakingOff(plane: Airplane, runway: Runway) extends Action(plane):
  private val neededSpeed: Double = 3
  private var started = false

  override def toString: String = "Taking off"

  def execute() =
    if !started then
      plane.location = runway.start.toCoord(coordPerTile)
      plane.bearing = Degrees(runway.direction.bearing)
      runway.airplanesWaitingForTakeoff.remove(runway.airplanesWaitingForTakeoff.indexOf(plane))
      runway.airplanesWaitingForTakeoff.foreach(n => n.location = (n.location.toGridPos(coordPerTile) - runway.direction).toCoord(coordPerTile) )
      started = true
    plane.fuel -= 0.2
    if overlappingRunwayPlaneWith(plane).nonEmpty then //if overlapping plane
      plane.action = Crashed(plane)
      overlappingRunwayPlaneWith(plane).foreach(n => n.action = Crashed(n) )
    if plane.location.toGridPos(coordPerTile).isSameAs(runway.end + runway.direction) then //at end of runway
      if plane.neededRunway < runway.length then
        plane.action = Crashed(plane)
      else
        plane.action = Leaving(plane)
    else if plane.speed < neededSpeed then
      plane.speed += (neededSpeed * neededSpeed / (2 * (plane.neededRunway) * coordPerTile))

class Leaving(plane: Airplane) extends Action(plane):

  override def toString: String = "Leaving the airport"

  def execute() =
    if plane.location.x < 0 || plane.location.y < 0 || plane.location.x > plane.game.grid.coordSize._1 || plane.location.y > plane.game.grid.coordSize._2 then
      plane.game.newScore("Plane left", 50)
      plane.game.airplanesOnMap.remove(plane.game.airplanesOnMap.indexOf(plane))
    plane.cruiseSpeed()
    plane.fuel -= 0.2