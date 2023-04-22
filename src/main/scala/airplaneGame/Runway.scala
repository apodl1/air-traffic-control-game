package airplaneGame

import airplaneGame.CompassDir.*

class Runway(index: Int, val start: GridPos, val end: GridPos):

  val direction: CompassDir =
    if start.x > end.x then
      West
    else if start.x < end.x then
      East
    else if start.y < end.y then
      South
    else
      North