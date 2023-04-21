package airplaneGame

import scala.io.StdIn.readInt

/* @main def generationTest() =
  val world = Grid(15, 15)
  var collapsed = 0
  while world.currentGrid.flatten.map( _.tile ).contains( None ) do
    world.collapseOne()
    collapsed += 1
  val printable = world.currentGrid.map( _.map( n => AllTiles.ascii(n.tile.getOrElse("nothing")) ) )

  printable.foreach( n => println(n.mkString(",")) )
  println(collapsed)
  println(world.numberOfRunways)
  println(world.runways.length)


*/


@main def generationTest() =
  val game = GameState(15, 15, 3, 60)
  val world = game.grid
  world.generate(4, (3, 3), Vector(3, 4))

  val printable = world.currentGrid.map( _.map( n => n.tile.getOrElse("********") ) )

  printable.foreach( n => println(n.mkString(",")) )
  world.runways.foreach( n => println(s"Start: ${n.start}, end: ${n.end}, ${n.direction}") )

  (1 to 20).foreach( n => game.tick() )
  (1 to 150).foreach( n => game.tick() )