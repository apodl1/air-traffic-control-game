package airplaneGame

import CompassDir.East

import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.util.Random

class Square(grid: Grid, val loc: GridPos):

  var tile = Option.empty[String]