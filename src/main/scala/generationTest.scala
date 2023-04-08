import scala.io.StdIn.readInt

@main def generationTest() =
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


