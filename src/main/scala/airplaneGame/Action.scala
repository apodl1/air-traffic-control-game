package airplaneGame

import scala.util.Random



trait Action(plane: Airplane):
  val coordPerTile = plane.game.coordPerTile
  val runwayActions: Vector[String] = Vector("Landing", "Expediting", "TakingOff")

  def overlappingRunwayPlaneWith(thisPlane: Airplane) =
    (plane.game.airplanesOnMap ++ plane.game.crashedPlanes)
      .filter( n => n != thisPlane && runwayActions.exists( m => n.action.getClass.getTypeName.contains(m) ) )
      .filter( n => 5 > math.abs(plane.location.x - n.location.x + (plane.location.y - n.location.y)) )

  def execute(): Unit


class Arriving(plane: Airplane) extends Action(plane): //puts plane on map
  var toArrive = 15

  def execute() =
    //println(s"Plane ${plane.id}, arriving in $toArrive")
    if toArrive == 0 then
      plane.action = GoingToRunway(plane, plane.game.grid.runways(Random.nextInt(plane.game.grid.numberOfRunways)))
      plane.location = Coord(Random.nextInt(plane.game.width * coordPerTile), 0) //TODO fix
      val indexInArriving =
        plane.game.airplanesToArrive.zipWithIndex.filter( _._1.id == plane.id ).head._2
      plane.game.airplanesToArrive.remove(indexInArriving)
      plane.game.airplanesOnMap.append(plane)
    else
      toArrive -= 1


class GoingToRunway(plane: Airplane, runway: Runway) extends Action(plane):
  val target: Coord = runway.start.toCoord(coordPerTile)

  override def toString: String = "Going to Runway #" + runway.index

  def execute() =
    //println(s"Plane ${plane.id}, at ${plane.location}")
    //println(plane.action.getClass.getTypeName)
    plane.fuel -= 0.2
    //if on target:
    if math.abs(plane.location.x - target.x) < 15 && math.abs(plane.location.y - target.y) < 15 then
      plane.location = target
      plane.action = Landing(plane, runway)
      plane.bearing = Degrees(runway.direction.bearing)
      return
    val bearingToRunway = plane.location.bearingTo(target)
    val bearingToRunwayDiff: Int = math.abs(plane.location.bearingTo(target) - plane.bearing.value)
    //how much to turn algo:
    val toTurn =
      if bearingToRunwayDiff <= 180 then
        (plane.maxTurn * (bearingToRunwayDiff.toDouble / 180)).ceil.toInt
      else
        (plane.maxTurn * ((360 - bearingToRunwayDiff.toDouble) / 180)).ceil.toInt
    //where to turn algo:
    if plane.bearing.value != bearingToRunway then
      if plane.location.x < target.x && ((plane.bearing.value < bearingToRunway) || (360 - (180 - bearingToRunway) < plane.bearing.value)) then
        plane.bearing += toTurn
      else if plane.location.x > target.x && (plane.bearing.value < bearingToRunway && plane.bearing.value > 180 - bearingToRunway) then
        plane.bearing += toTurn
      else
        plane.bearing -= toTurn
    /*if bearingToTargetDiff > 0 && bearingToTargetDiff < 180 then
      plane.bearing += toTurn
    else
    if bearingToTargetDiff <= 180 && plane.location.bearingTo(target) < plane.bearing.value then
      plane.bearing -= toTurn
    else if bearingToTargetDiff <= 180 && plane.location.bearingTo(target) > plane.bearing.value then
      plane.bearing += toTurn
    else if bearingToTargetDiff > 180 && plane.location.bearingTo(target) > plane.bearing.value then
      plane.bearing -= toTurn
    else if bearingToTargetDiff > 180 && plane.location.bearingTo(target) < plane.bearing.value then
      plane.bearing += toTurn */


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
  val neededSpeed = 1
  val originalSpeed = plane.speed

  override def toString: String = "Landing at Runway #" + runway.index

  def atEndOfRunway = 10 > math.abs(plane.location.x - runway.end.toCoord(coordPerTile).x + (plane.location.y - runway.end.toCoord(coordPerTile).y))

  def execute() =
    if overlappingRunwayPlaneWith(plane).nonEmpty then //if overlapping plane
      plane.action = Crashed(plane)
      overlappingRunwayPlaneWith(plane).foreach(n => n.action = Crashed(n) )
    if plane.speed == neededSpeed && atEndOfRunway then //landed
      plane.location = runway.arrivingWaitArea.toCoord(coordPerTile)
      plane.action = TaxiingToGate(plane)
      plane.speed = 0
    else if atEndOfRunway then //too fast at end
      plane.action = Crashed(plane)
    if plane.speed > neededSpeed then
      plane.speed = math.max(1, plane.speed - ((4.0 * 4 / (2 * plane.neededRunway * coordPerTile))))

class Expediting(plane: Airplane) extends Action(plane):
  var beenOffFor = 0
  var isOff = false

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
  var crashedFor = 0

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
  val timeToTaxi = 150
  var timer = 0
  var taxiing = false
  var gate: Option[Gate] = None

  var description = "Waiting for free gate"

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


class Boarding(plane: Airplane) extends Action(plane):
  val timeToBoard = 150
  var timer = 0
  val tenPercent = timeToBoard / 10
  def percentiles = timer / tenPercent

  val sharps = "|##########|"
  val dots = "__________|"
  val start = "Boarding, progress: "
  var description = "Boarding, time to board: " + (timeToBoard - timer) + "\nProgress: " + sharps.head + dots

  override def toString: String = description

  def execute() =
    if timer == timeToBoard then
      plane.fuel = plane.maxFuel
      plane.action = Boarded(plane)
    else
      timer += 1
    if timer <= timeToBoard && timer % tenPercent == 0 then
      description = start + sharps.take(percentiles + 1) + dots.drop(percentiles)

class Boarded(plane: Airplane) extends Action(plane):
  var timer = 0
  val description = "Boarded, awaiting order to taxi"
  override def toString: String = description

  def execute() =
    timer += 1



class TaxiingToRunway(plane: Airplane, runway: Runway) extends Action(plane):
  val timeToTaxi = 150
  var timer = 0

   override def toString: String = "Taxiing to runway #" + runway.index + ". Time left: " + (timeToTaxi - timer)

  def execute() =
    plane.location = Coord(-100, -100)
    if timer == timeToTaxi then
      var whereToDraw = runway.departingWaitArea - runway.direction
      runway.airplanesWaitingForTakeoff.foreach(n => whereToDraw = whereToDraw + runway.direction )
      plane.location = whereToDraw.toCoord(coordPerTile)
      plane.action = WaitingOnRunway(plane, runway)
    else
      timer += 1

class WaitingOnRunway(plane: Airplane, runway: Runway) extends Action(plane):
  var timer = 0

  def execute(): Unit =
    if timer == 0 then
      runway.airplanesWaitingForTakeoff.enqueue(plane)
    timer += 1

class TakingOff(plane: Airplane, runway: Runway) extends Action(plane):
  val neededSpeed: Double = 3
  var started = false

  def execute() =
    if !started then
      plane.location = runway.start.toCoord(coordPerTile)
      plane.bearing = Degrees(runway.direction.bearing)
      runway.airplanesWaitingForTakeoff.dequeue()
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

  def execute() =
    if plane.location.x < 0 || plane.location.y < 0 || plane.location.x > plane.game.grid.coordSize._1 || plane.location.y > plane.game.grid.coordSize._2 then
      plane.game.airplanesOnMap.remove(plane.game.airplanesOnMap.indexOf(plane))
    plane.cruiseSpeed()
    plane.fuel -= 0.2