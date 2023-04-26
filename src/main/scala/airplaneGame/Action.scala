package airplaneGame

import scala.util.Random
import airplaneGame.CompassDir

import scala.collection.mutable.ArrayBuffer
import scala.swing.Dialog


//all classes derive from Action-trait, which provides the interface. The calss names are also used by GUI, to determine the state of the aircraft
trait Action(plane: Airplane):
  val coordPerTile = plane.game.coordPerTile //shorthand
  val runwayActions: Vector[String] = Vector("Landing", "Expediting", "TakingOff") //actions which happen on the runway
  var boarded = false //flag for the GUI

  //function for finding possible overlapping planes. Returns empty list if none
  def overlappingRunwayPlaneWith(thisPlane: Airplane): ArrayBuffer[Airplane] =
    (plane.game.airplanesOnMap ++ plane.game.crashedPlanes)
      .filter(n => n != thisPlane && runwayActions.exists(m => n.action.getClass.getTypeName.contains(m)))
      .filter(n => 5 > math.abs(plane.location.x - n.location.x + (plane.location.y - n.location.y)))

  //implimented by classes
  def execute(): Unit

//puts plane on the map after set time
class Arriving(plane: Airplane) extends Action(plane):
  private val toArrive = 150
  private var timer = 0
  private var printed = false

  def execute() =
    //println(s"Plane ${plane.id}, arriving in $toArrive")
    timer += 1
    if toArrive == timer then //go to map
      plane.game.newArrivalMessage("Plane #" + plane.id + " requesting permission to land\n")
      plane.action = GoingToRunway(plane, plane.game.grid.runways(Random.nextInt(plane.game.grid.numberOfRunways)))
      plane.location = Coord(Random.nextInt(plane.game.width * coordPerTile), 0) //TODO fix
      val indexInArriving =
        plane.game.airplanesToArrive.zipWithIndex.filter( _._1.id == plane.id ).head._2
      plane.game.airplanesToArrive.remove(indexInArriving)
      plane.game.airplanesOnMap.append(plane)
    if timer / toArrive.toDouble > 0.5 && !printed then //print message
      printed = true
      plane.game.newArrivalMessage("Plane #" + plane.id + s" arriving in ${toArrive / 2}\n")


//perhaps the most important class, hosts the pathfinding alorithm for going to runway
class GoingToRunway(plane: Airplane, runway: Runway) extends Action(plane):
  private val runwayStart: Coord = runway.start.toCoord(coordPerTile)
  //flags
  private var oriented = false
  private var clearedForEntry = false
  //point in front of runway
  private val entryPoint: Coord = (runway.start - runway.direction - runway.direction).toCoord(coordPerTile)

  //point to the side of the runway
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

  def goToPoint(target: Coord): Unit =
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
      clearedForEntry = false
      oriented = false
    else if clearedForEntry then
      goToPoint(runwayStart)
    else if conditionToEntry && oriented then
      clearedForEntry = true
      goToPoint(runwayStart)
    else if oriented then
      goToPoint(entryPoint)
    else if conditionToOrient then
      oriented = true
      goToPoint(entryPoint)
    else
      goToPoint(orientationPoint)


//circling classes
class CirclingLeft(plane: Airplane) extends Action(plane):
  override def toString: String = "Circling left. Remaining fuel: " + plane.fuelToDisplay

  def execute() =
    plane.bearing -= plane.maxTurn / 4
    plane.fuel -= plane.fuelConsumption

class CirclingRight(plane: Airplane) extends Action(plane):
  override def toString: String = "Circling right. Remaining fuel: " + plane.fuelToDisplay

  def execute() =
    plane.bearing += plane.maxTurn / 4
    plane.fuel -= plane.fuelConsumption


class Landing(plane: Airplane, runway: Runway) extends Action(plane):
  private val neededSpeed = 1

  override def toString: String = "Landing at Runway #" + runway.index

  def atEndOfRunway: Boolean = 10 > math.abs(plane.location.x - runway.end.toCoord(coordPerTile).x + (plane.location.y - runway.end.toCoord(coordPerTile).y))

  def execute() =
    if overlappingRunwayPlaneWith(plane).nonEmpty then //if overlapping plane -> crash
      plane.action = Crashed(plane)
      overlappingRunwayPlaneWith(plane).foreach(n => n.action = Crashed(n) )
    if (plane.speed == neededSpeed || plane.neededRunway <= runway.length) && atEndOfRunway then //if landed -> land (go to waiting area and change action)
      var whereDraw = runway.arrivingWaitArea
      runway.airplanesWaitingForGate.foreach( n => whereDraw = whereDraw - runway.direction )
      runway.airplanesWaitingForGate.enqueue(plane)
      plane.location = whereDraw.toCoord(coordPerTile)
      plane.action = TaxiingToGate(plane)
      plane.speed = 0
    else if atEndOfRunway then //if too fast at end of runway (an not upposed to land) -> crash
      plane.action = Crashed(plane)
    if plane.speed > neededSpeed then //if not at required speed -> brake amount derived from max speed and needed runway, up to needed speed
      plane.speed = math.max(neededSpeed, plane.speed - ((plane.maxSpeed * plane.maxSpeed / (2 * plane.neededRunway * coordPerTile))))

//take off while landing
class Expediting(plane: Airplane) extends Action(plane):
  private var beenOffFor = 0
  private var isOff = false

  def execute(): Unit =
    if overlappingRunwayPlaneWith(plane).nonEmpty then //if overlapping plane -> crash
      plane.action = Crashed(plane)
      overlappingRunwayPlaneWith(plane).foreach(n => n.action = Crashed(n) )
    if plane.speed > 3 then //-> when at sufficient speed -> set flag
      isOff = true
    if isOff then //if flag -> avance timer
      beenOffFor += 1
    if beenOffFor > 40 then //if timer ifficiently advanced -> circle
      plane.action = CirclingLeft(plane)
    if !isOff then //otherwise accelerate
      plane.speed += 0.2


class Crashed(plane: Airplane) extends Action(plane):
  private var crashedFor = 0

  def showEndMessage(): Unit =
    Dialog.showMessage(
      title = "Oh no",
      message = "A plane crahed! You have done a final mistake." +
        "\nThere were " + plane.game.crashedPlanes.foldLeft(0)((sum, plane) => sum + plane.passengers) + " people on those planes. Your game is over"
    )

  def execute() =
    if crashedFor == 0 then //when just introduced, change the status of plane in gameState
      if plane.game.airplanesOnMap.lift(plane.game.airplanesOnMap.zipWithIndex.filter( _._1.id == plane.id ).head._2).isDefined && plane.game.airplanesOnMap.zipWithIndex.exists(_._1.id == plane.id) then
        plane.game.airplanesOnMap.remove(plane.game.airplanesOnMap.zipWithIndex.filter( _._1.id == plane.id ).head._2)
      plane.game.crashedPlanes.enqueue(plane)
    crashedFor += 1
    if crashedFor > 30 then
      showEndMessage()
      sys.exit()
    if plane.speed != 0 then //slow down
      plane.speed = math.max(0, plane.speed - 0.2)
    if crashedFor > 40 then //remove after time
      plane.game.crashedPlanes.dequeue()



class TaxiingToGate(plane: Airplane) extends Action(plane):
  private val timeToTaxi = 70
  private var timer = 0
  private var taxiing = false
  private var gate: Option[Gate] = None
  //initial value:
  private var description = "Waiting for free gate"

  override def toString: String = description

  def initTaxi() =
    plane.speed = 0
    plane.location = Coord(-1000, -1000) //move outside map

  def execute() =
    if !taxiing && !plane.game.grid.gates.forall( _.plane.isDefined ) then //if not taxiing and if space at gates -> start taxiing, otherwise does nothing
      plane.game.grid.runways.filter( _.airplanesWaitingForGate.contains(plane) ).foreach( _.airplanesWaitingForGate.dequeue() )
      val gateToAssign = plane.game.grid.gates.filter( _.plane.isEmpty ).head
      gateToAssign.plane = Some(plane)
      gate = Some(gateToAssign)
      initTaxi()
      taxiing = true
      description = "Taxiing to gate"
    if timer == timeToTaxi then //end taxiing at sufficient time
      plane.action = Boarding(plane)
      plane.location = gate.get.loc.toCoord(coordPerTile) + Coord(0, 30)
      plane.bearing = Degrees(0)
    else if taxiing then //taxi
      timer += 1
      description = "Taxiing to gate, time left: " + (timeToTaxi - timer)


class Boarding(plane: Airplane) extends Action(plane):
  private val timeToBoard = 100
  private var timer = 0
  private val tenPercent = timeToBoard / 10
  def percentiles = timer / tenPercent

  //visualizer
  private val sharps = "|##########|"
  private val dots = "__________|"
  private val start = "Boarding, progress: "
  private var description = "Boarding, time to board: " + (timeToBoard - timer) + "\nProgress: " + sharps.head + dots

  override def toString: String = description

  def execute() =
    if timer == timeToBoard then //if boarded -> change action
      plane.fuel = plane.startFuel
      plane.passengers = Random.nextInt(170)
      plane.game.newScore(plane.passengers + " passengers arrived.", plane.passengers)
      plane.action = Boarded(plane)
    else
      timer += 1
    if timer <= timeToBoard && timer % tenPercent == 0 then //change visualizer depending on condition
      description = start + sharps.take(percentiles + 1) + dots.drop(percentiles)

class Boarded(plane: Airplane) extends Action(plane):
  private var timer = 0
  private val description = "Boarded, awaiting order to taxi"
  override def toString: String = description

  //awaits user input without doing anything
  def execute() =
    boarded = true
    timer += 1



class TaxiingToRunway(plane: Airplane, runway: Runway) extends Action(plane):
  private val timeToTaxi = 70
  private var timer = 0

  override def toString: String = "Taxiing to runway #" + runway.index + ". Time left: " + (timeToTaxi - timer)

  def execute() =
    plane.game.grid.gates.filter( _.plane == Some(plane) ).foreach( _.plane = None ) //clears the gate
    plane.location = Coord(-1000, -1000) //goes outside the map
    if timer == timeToTaxi then //if sufficient time -> change action and update location
      var whereToDraw = runway.departingWaitArea
      runway.airplanesWaitingForTakeoff.foreach(n => whereToDraw = whereToDraw + runway.direction ) //changes the location depending on the runways awaiting planes
      plane.location = whereToDraw.toCoord(coordPerTile)
      plane.action = WaitingOnRunway(plane, runway)
    else
      timer += 1

class WaitingOnRunway(plane: Airplane, runway: Runway) extends Action(plane):
  private var timer = 0

  override def toString: String = "Waiting for permission to take off"

  def execute(): Unit =
    if timer == 0 then //if jut initialized, add to runway array. Otherwise do nothing
      runway.airplanesWaitingForTakeoff.append(plane)
    timer += 1

class TakingOff(plane: Airplane, runway: Runway) extends Action(plane):
  private val neededSpeed: Double = 3
  private var started = false //initilization flag

  override def toString: String = "Taking off"

  def execute() =
    if !started then //initialize on firt pass, set location, remove from buffer and move othe rwaiting planes
      plane.location = runway.start.toCoord(coordPerTile)
      plane.bearing = Degrees(runway.direction.bearing)
      runway.airplanesWaitingForTakeoff.remove(runway.airplanesWaitingForTakeoff.indexOf(plane))
      runway.airplanesWaitingForTakeoff.foreach(n => n.location = (n.location.toGridPos(coordPerTile) - runway.direction).toCoord(coordPerTile) )
      started = true
    plane.fuel -= plane.fuelConsumption
    if overlappingRunwayPlaneWith(plane).nonEmpty then //if overlapping plane -> crash
      plane.action = Crashed(plane)
      overlappingRunwayPlaneWith(plane).foreach(n => n.action = Crashed(n) )
    if plane.location.toGridPos(coordPerTile).isSameAs(runway.end + runway.direction) then //if at end of runway
      if plane.neededRunway < runway.length then //if runway too short -> crash
        plane.action = Crashed(plane)
      else //else -> leave
        plane.action = Leaving(plane)
    else if plane.speed < neededSpeed then //accelerate
      plane.speed += (neededSpeed * neededSpeed / (2 * (plane.neededRunway) * coordPerTile))

//simple class for guiding the plane out
class Leaving(plane: Airplane) extends Action(plane):

  override def toString: String = "Leaving the airport"

  def execute() =
    if plane.location.x < 0 || plane.location.y < 0 || plane.location.x > plane.game.grid.coordSize._1 || plane.location.y > plane.game.grid.coordSize._2 then //if plane off map -> despawn
      plane.game.newScore("Plane left", plane.passengers)
      plane.game.airplanesOnMap.remove(plane.game.airplanesOnMap.indexOf(plane))
    plane.cruiseSpeed()
    plane.fuel -= plane.fuelConsumption