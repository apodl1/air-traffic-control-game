class GridPos(val x: Int, val y: Int):

  def neighborOption(direction: CompassDir, gridSize: (Int, Int)): Option[GridPos] =
    val neighborPos = GridPos(x + direction.xStep, y + direction.yStep)
    if neighborPos.x > gridSize._1 || neighborPos.x < 0 || neighborPos.y > gridSize._2 || neighborPos.y < 0
      then None
    else
      Some(neighborPos)