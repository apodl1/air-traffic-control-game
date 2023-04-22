package airplaneGame

import java.awt.{Dimension, Image}
import javax.swing.{JLayeredPane, OverlayLayout, SwingUtilities, SwingWorker}
import scala.swing.*
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import java.awt.Color.RED
import java.awt.geom.{Ellipse2D, Rectangle2D}


object AirplaneGame extends SimpleSwingApplication:

  val gridSizeX = 18
  val gridSizeY = 18
  val bufferSize = 3
  val coordPerGridPos = 40

  val numberOfRunways = 2
  val terminalSize = (5, 5)
  val runwayLengths = Vector(5, 6)

  val planeSize = (30, 30)


  val game: GameState = GameState(gridSizeX, gridSizeY, bufferSize, coordPerGridPos)

  game.grid.generate(numberOfRunways, terminalSize, runwayLengths)

  //game.grid.runways.foreach( n => println(n.start) )
  //game.grid.currentGrid.flatten.filter( _.tile.contains("S"

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
  gridImageG.dispose()

  def getNewGridImage: BufferedImage =
    val newGridImage: BufferedImage = BufferedImage(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos, BufferedImage.TYPE_INT_ARGB)
    val newGridImageG = newGridImage.getGraphics.asInstanceOf[Graphics2D]
    newGridImageG.drawImage(gridImage, 0, 0, null)
    newGridImageG.dispose()
    newGridImage


  val planeImage = ImageIO.read(new File("Tiles/Plane.png")).getScaledInstance(planeSize._1, planeSize._2, java.awt.Image.SCALE_REPLICATE)
  val crashedImage = ImageIO.read(new File("Tiles/Crashed.png")).getScaledInstance(planeSize._1, planeSize._2, java.awt.Image.SCALE_REPLICATE)
  //planeImage.getGraphics.asInstanceOf[Graphics2D].scale(10, 10)
  var planesOnMap: BufferedImage = gridImage

  def getTurnedPlane(bearing: Int): BufferedImage =
    val newPlaneImage: BufferedImage = BufferedImage(planeSize._1, planeSize._2, BufferedImage.TYPE_INT_ARGB)
    val newPlaneImageG = newPlaneImage.getGraphics.asInstanceOf[Graphics2D]
    newPlaneImageG.rotate(bearing.toDouble.toRadians - 90.toRadians, planeSize._1 / 2, planeSize._2 / 2)
    newPlaneImageG.drawImage(planeImage, 0, 0, null)
    newPlaneImageG.dispose()
    newPlaneImage

  def drawPlanes(): Unit =
      planesOnMap = getNewGridImage
      val g = planesOnMap.getGraphics.asInstanceOf[Graphics2D]
      game.airplanesOnMap.foreach( n => g.drawImage(getTurnedPlane(n.bearing.value), n.location.x, n.location.y, null) )
      game.crashedPlanes.foreach( n => g.drawImage(crashedImage, n.location.x, n.location.y, null) )
      //g.drawImage(planeImage, 100, 100, 20, 20, null)
      g.setColor(RED)
      val runway = game.grid.runways.head.start
      //println("Runway: "+ runway.toCoord(coordPerGridPos))
      g.draw(Ellipse2D.Double(runway.x * coordPerGridPos, runway.y * coordPerGridPos, 10, 10))
      g.dispose()
  end drawPlanes


  var selectedPlane: Option[Airplane] = None

  val planeInfo = new TextArea(15, 100):
    maximumSize = new Dimension(150, 300)
    editable = false
    wordWrap = true
    lineWrap = true

  planeInfo.text = PlaneTextToDisplay(None).text


  var mouseX = 0
  var mouseY = 0

  val mapPanel = new Panel:
    preferredSize = new Dimension(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos)
    override def paintComponent(g: Graphics2D): Unit =
      super.paintComponent(g)
      g.drawImage(planesOnMap, 0, 0, null)

    this.listenTo(mouse.moves)
    this.listenTo(mouse.clicks)

    this.reactions += {
      case event.MouseMoved(_, point: java.awt.Point, _) =>
        mouseX = point.x
        mouseY = point.y
      case event.MouseClicked(_, _, _, _, _) =>
        val clickCoord = Coord(mouseX, mouseY)
        val possiblePlane = game.planeAtCoord(clickCoord)
        val possibleRunway = game.runwayAtCoord(clickCoord)
        if possiblePlane.isEmpty && possibleRunway.isEmpty then
          selectedPlane = None
        else if possibleRunway.isEmpty then
          possiblePlane.foreach( n => selectedPlane = Some(n))
        else
          selectedPlane.foreach( _.action = GoingToRunway(selectedPlane.get, possibleRunway.get) )
        planeInfo.text = PlaneTextToDisplay(selectedPlane).text
    }

  val buttonDim = Dimension(30, 10)
  val slowButton = new Button("Slow speed"):
    maximumSize = buttonDim
    action = new Action("Slow speed"):
      def apply() = (selectedPlane.foreach( _.slowSpeed() ))
  val cruiseButton = new Button("Cruise speed"):
    maximumSize = buttonDim
    action = new Action("Cruise speed"):
      def apply() = (selectedPlane.foreach( _.cruiseSpeed() ))
  val fastButton = new Button("Fast speed"):
    maximumSize = buttonDim
    action = new Action("Fast speed"):
      def apply() = (selectedPlane.foreach( _.fastSpeed() ))

  var rightPanel = new BoxPanel(Orientation.Vertical):
    preferredSize = new Dimension(150, gridSizeY * coordPerGridPos)
    contents += planeInfo
    contents += Swing.VStrut(10)
    contents += new BorderPanel:
      add(slowButton, BorderPanel.Position.West)
    contents += Swing.VStrut(10)
    contents += new BorderPanel:
      add(cruiseButton, BorderPanel.Position.West)
    contents += Swing.VStrut(10)
        contents += new BorderPanel:
      add(fastButton, BorderPanel.Position.West)
    contents += Swing.VStrut(10)

  /*var textPanel = new BoxPanel(Orientation.Vertical):
    preferredSize = new Dimension(100, gridSizeY * coordPerGridPos)
    contents += new Label("Location: ")
    contents += new Label("Bearing: ")
    contents += new Label("Speed: ")
    contents += new Label("Fuel: ")
    contents += new Label("Origin: ")
    contents += new Label("Action: ")

  def updateTextPanel(planeToDisplay: Option[Airplane]): Unit =
    textPanel = new BoxPanel(Orientation.Vertical):
      preferredSize = new Dimension(100, gridSizeY * coordPerGridPos)
    if planeToDisplay.isDefined then
      println("updated")
      val plane = planeToDisplay.get
      textPanel.contents += new Label("Location: " + plane.location)
      textPanel.contents += new Label("Bearing: " + plane.bearing)
      textPanel.contents += new Label("Speed: " + plane.speed)
      textPanel.contents += new Label("Fuel: " + plane.fuel)
      textPanel.contents += new Label("Origin: " + plane.origin)
      textPanel.contents += new Label("Needed runway: " + plane.neededRunway)
      textPanel.contents += new Label("Action: " + plane.action)*/

  val mapTextPanel = new BoxPanel(Orientation.Horizontal):
    contents += mapPanel
    contents += rightPanel


  def tick(): Unit =
    game.tick()
    drawPlanes()
    mapPanel.repaint()
    planeInfo.text = PlaneTextToDisplay(selectedPlane).text
    //println("" + mouseX + "," + mouseY)


  def top = new MainFrame:
    title = "Air Traffic Controller"

    javax.swing.Timer(200, e => tick()).start()

    contents = mapTextPanel