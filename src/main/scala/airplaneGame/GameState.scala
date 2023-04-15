package airplaneGame

class GameState(val width: Int, val height: Int, val bufferSize: Int, val coordPerTile: Int):
  
  val grid = Grid(width, height, bufferSize, coordPerTile)

  //placeholder class