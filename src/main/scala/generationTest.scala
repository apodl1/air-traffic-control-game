import scala.io.StdIn.readInt

@main def generationTest() =
  val world = Grid(10, 10)
  var collapsed = 0
  while world.currentGrid.flatten.map( _.tile ).contains( None ) do
    world.collapseOne()
    collapsed += 1
  world.currentGrid.map( _.map( _.tile.getOrElse("nothing") )).foreach( n => println(n.mkString(",").appended('\n')) )
  println(collapsed)