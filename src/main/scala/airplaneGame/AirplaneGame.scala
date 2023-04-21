package airplaneGame

import java.awt.{Dimension, Image}
import javax.swing.{JLayeredPane, OverlayLayout, SwingUtilities, SwingWorker}
import scala.swing.*
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon


object AirplaneGame extends SimpleSwingApplication:

  val gridSizeX = 18
  val gridSizeY = 18
  val bufferSize = 3
  val coordPerGridPos = 40

  val numberOfRunways = 2
  val terminalSize = (5, 5)
  val runwayLengths = Vector(5, 6)


  val game: GameState = GameState(gridSizeX, gridSizeY, bufferSize, 60)

  game.grid.generate(numberOfRunways, terminalSize, runwayLengths)

  /*val drawableGrid = new GridPanel(gridSizeX, gridSizeY):
    game.grid.renderable
      .map( tile => new Label{icon = AllTiles.images(tile)} )
      .foreach( contents += _ )*/

  val gridImage: BufferedImage = BufferedImage(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos, BufferedImage.TYPE_INT_ARGB)
  val gridImageG = gridImage.getGraphics.asInstanceOf[Graphics2D]
  game.grid.currentGrid.zipWithIndex.foreach( y =>
    y._1.zipWithIndex.foreach( x =>
      gridImageG.drawImage(ImageIO.read(File("./Tiles/" + x._1.tile.getOrElse("grounddd") + ".png"))
        .getScaledInstance(AirplaneGame.coordPerGridPos, AirplaneGame.coordPerGridPos, Image.SCALE_DEFAULT), x._2 * coordPerGridPos, y._2 * coordPerGridPos, null) ) )

  val planeImage = ImageIO.read(new File("Tiles/Plane.png")).getScaledInstance(20, 20, java.awt.Image.SCALE_REPLICATE)
  //planeImage.getGraphics.asInstanceOf[Graphics2D].scale(10, 10)
  var planesOnMap: BufferedImage = gridImage

  def drawPlanes(): Unit =
      planesOnMap = gridImage
      val g = planesOnMap.getGraphics.asInstanceOf[Graphics2D]
      game.airplanesOnMap.foreach( n => g.drawImage(planeImage, n.location.x, n.location.y, null) )
      //g.drawImage(planeImage, 100, 100, 20, 20, null)
  end drawPlanes

  val drawingPanel = new Panel:
    preferredSize = new Dimension(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos)
    override def paintComponent(g: Graphics2D): Unit =
      super.paintComponent(g)
      g.drawImage(planesOnMap, 0, 0, null)

  def tick(): Unit =
    game.tick()
    //drawPlanes()
    //top.repaint()
    drawPlanes()
    drawingPanel.repaint()

  def top = new MainFrame:
    title = "Air Traffic Controller"


    javax.swing.Timer(300, e => tick()).start()

    contents = drawingPanel