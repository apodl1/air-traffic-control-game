package airplaneGame

class PlaneTextToDisplay(maybePlane: Option[Airplane]):

  val text: String =
    if maybePlane.isEmpty then
      "Currently selected: [None]" +
      "\nLocation: " +
      "\nBearing: " +
      "\nSpeed: " +
      "\nFuel: " +
      "\nOrigin: " +
      "\nNeeded runway: " +
      "\nAction: "
    else
      val plane = maybePlane.get
      "Currently selected: plane #" + plane.id +
      "\nLocation: " + plane.location +
      "\nBearing: " + plane.bearing +
      "\nSpeed: " + math.BigDecimal(plane.speed).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble +
      "\nFuel: " + plane.fuelToDisplay +
      "\nOrigin: " + plane.origin +
      "\nNeeded runway: " + plane.neededRunway +
      "\nAction: " + plane.action

class ArrivingPlaneMessage(plane: Airplane):

  val message: String =
    "Plane #" + plane.id + ", from: " + plane.origin +
    "\nPassengers: " + plane.passengers +
    "\nTime to arrival: 15" +
    "\nFuel on arrival: " + plane.fuel +
    "\n"


class AirportInfo(game: GameState):

  def gateStatus(gate: Gate): String =
    if gate.plane.isEmpty then
      "Empty"
    else if gate.plane.forall( _.action.boarded ) then
      "Plane #" + gate.plane.get.id + " boarded"
    else
      "Plane #" + gate.plane.get.id + " boarding"

  val text: String =
    game.grid.runways.map( n => "Runway #" + n.index + ": Available").mkString("\n") + "\n\n" +
    game.grid.gates.map( n => "Gate #" + n.index + ": " + gateStatus(n)).mkString("\n") +
    "\n\nScore:" + game.score + "\n" +
    game.latestScoreMessage