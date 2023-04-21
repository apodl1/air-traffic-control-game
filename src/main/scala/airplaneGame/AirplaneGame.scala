package airplaneGame

import javax.swing.{JLayeredPane, OverlayLayout}
import scala.swing.*
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object AirplaneGame extends SimpleSwingApplication:

  val gridSizeX = 25
  val gridSizeY = 15
  val bufferSize = 3
  val coordPerGridPos = 40

  val numberOfRunways = 2
  val terminalSize = (5, 5)
  val runwayLengths = Vector(5, 6)


  val game: GameState = GameState(gridSizeX, gridSizeY, bufferSize, 60)

  game.grid.generate(numberOfRunways, terminalSize, runwayLengths)


  def top = new MainFrame:
    title = "Air Traffic Controller"

    javax.swing.Timer(300, e => game.tick()).start()


    val drawableGrid = new GridPanel(gridSizeX, gridSizeY):
      game.grid.renderable
        .map( tile => new Label{icon = AllTiles.images(tile)} )
        .foreach( contents += _ )

    val image = ImageIO.read(new File("Tiles/Plane.png"))
    val drawingPanel = new Panel:
      preferredSize = new Dimension(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos)
      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)
        //g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
        //g.drawImage(image, x, y, 20, 20, null)
        g.drawImage(image, 100, 100, 20, 20, null)

    contents = new FlowPanel():
      contents += drawingPanel
      contents += drawableGrid

    size = new Dimension(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos)