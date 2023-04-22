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
      "\nSpeed: " + plane.speed +
      "\nFuel: " + plane.fuel.floor +
      "\nOrigin: " + plane.origin +
      "\nNeeded runway: " + plane.neededRunway +
      "\nAction: " + plane.action