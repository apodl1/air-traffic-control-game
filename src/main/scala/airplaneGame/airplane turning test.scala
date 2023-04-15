package airplaneGame

import scala.math

class test(var coord: (Int, Int)):
  var vel = math.Pi / 4
  var dir = 0.0
  var speed = 100

  def move() =
    coord = (coord._1 + (math.sin(dir) * speed).toInt, coord._2 + (math.cos(dir) * speed).toInt)

  def turn() =
    dir = dir + vel



@main def a() =
  val a = test((0, 0))
  a.move()
  a.turn()
  a.move()
  println(a.coord)
  println(a.dir)