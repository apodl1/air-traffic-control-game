package airplaneGame

import java.awt.{Dimension, Image}
import javax.swing.{JLayeredPane, OverlayLayout, SwingUtilities, SwingWorker}
import scala.swing.*
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import java.awt.Color.{RED, GREEN}
import java.awt.geom.{Ellipse2D, Rectangle2D}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

//main application
object AirplaneGame extends SimpleSwingApplication:

  //contants
  val gridSizeX = 18 //TODO min values
  val gridSizeY = 18
  val bufferSize = 3
  val coordPerGridPos = 40

  val numberOfRunways = 2 //min (suggested) value: 1
  val terminalSize = (6, 5) //min value: (3, 3)
  val runwayLengths = Vector(5, 6) //min value: 3

  val planeSize = (30, 30)

  //create GameState object
  val game: GameState = GameState(gridSizeX, gridSizeY, bufferSize, coordPerGridPos)
  //generate grid for GameState
  game.grid.generate(numberOfRunways, terminalSize, runwayLengths)

  //get and draw the grid
  val gridImage: BufferedImage = BufferedImage(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos, BufferedImage.TYPE_INT_ARGB)
  val gridImageG = gridImage.getGraphics.asInstanceOf[Graphics2D]
  game.grid.currentGrid.zipWithIndex.foreach( y =>
    y._1.zipWithIndex.foreach( x =>
      gridImageG.drawImage(ImageIO.read(File("./Tiles/" + x._1.tile.getOrElse("grounddd") + ".png"))
        .getScaledInstance(AirplaneGame.coordPerGridPos, AirplaneGame.coordPerGridPos, Image.SCALE_DEFAULT), x._2 * coordPerGridPos, y._2 * coordPerGridPos, null) ) )
  gridImageG.setColor(GREEN)
  game.grid.runways.foreach( n => gridImageG.drawString("#" + n.index, (n.start - n.direction).toCoord(coordPerGridPos).x, (n.start - n.direction).toCoord(coordPerGridPos).y) )
  gridImageG.dispose()

  //get new grid image to draw planes on
  def getNewGridImage: BufferedImage =
    val newGridImage: BufferedImage = BufferedImage(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos, BufferedImage.TYPE_INT_ARGB)
    val newGridImageG = newGridImage.getGraphics.asInstanceOf[Graphics2D]
    newGridImageG.drawImage(gridImage, 0, 0, null)
    newGridImageG.dispose()
    newGridImage

  //get plane images
  val smallPlaneImage = ImageIO.read(new File("Tiles/smallPlane.png")).getScaledInstance(planeSize._1, planeSize._2, java.awt.Image.SCALE_REPLICATE)
  val mediumPlaneImage = ImageIO.read(new File("Tiles/mediumPlane.png")).getScaledInstance(planeSize._1, planeSize._2, java.awt.Image.SCALE_REPLICATE)
  val bigPlaneImage = ImageIO.read(new File("Tiles/bigPlane.png")).getScaledInstance(planeSize._1, planeSize._2, java.awt.Image.SCALE_REPLICATE)
  //graphic for crahed plane
  val crashedImage = ImageIO.read(new File("Tiles/Crashed.png")).getScaledInstance(planeSize._1, planeSize._2, java.awt.Image.SCALE_REPLICATE)
  //initilaize planesOnMap to grid image
  var planesOnMap: BufferedImage = gridImage

  //helper for rotating plane images for drawing. Rotates the image by paramater bearing and chooses image based on model
  def getTurnedPlane(bearing: Int, model: String): BufferedImage =
    val newPlaneImage: BufferedImage = BufferedImage(planeSize._1, planeSize._2, BufferedImage.TYPE_INT_ARGB)
    val newPlaneImageG = newPlaneImage.getGraphics.asInstanceOf[Graphics2D]
    newPlaneImageG.rotate(bearing.toDouble.toRadians - 90.toRadians, planeSize._1 / 2, planeSize._2 / 2)
    if model == "Airbus 330" then
      newPlaneImageG.drawImage(smallPlaneImage, 0, 0, null)
    else if model == "Boeing 737-800" then
      newPlaneImageG.drawImage(mediumPlaneImage, 0, 0, null)
    else if model == "Boeing 777X" then
      newPlaneImageG.drawImage(bigPlaneImage, 0, 0, null)
    newPlaneImageG.dispose()
    newPlaneImage

  //draws planes to new grid image
  def drawPlanes(): Unit =
      planesOnMap = getNewGridImage
      val g = planesOnMap.getGraphics.asInstanceOf[Graphics2D]
      //loops through GameStates buffers
      game.airplanesOnMap.foreach( n => g.drawImage(getTurnedPlane(n.bearing.value, n.model), n.location.x - planeSize._1 / 2, n.location.y - planeSize._2 / 2, null) )
      game.crashedPlanes.foreach( n => g.drawImage(crashedImage, n.location.x, n.location.y, null) )
      //draws a red ellipse around selected plane (if it is defined)
      g.setColor(RED)
      selectedPlane.foreach( n => g.draw(Ellipse2D.Double(n.location.x - planeSize._1 / 2 - 5, n.location.y - planeSize._2 / 2 - 5, 40, 40)) )
      //selectedPlane.foreach( n => g.draw(Ellipse2D.Double(n.location.x - planeSize._1 / 2 - 5, n.location.y - planeSize._2 / 2 - 5, 40, 40)) )
      g.dispose()
  end drawPlanes

  //var to store selected plane if defined
  var selectedPlane: Option[Airplane] = None

  //vers to store mouse positions
  var mouseX = 0
  var mouseY = 0

  //swing panel for the game map. Draws planesOnMap after is has been updated by DrawPlanes()
  val mapPanel = new Panel:
    preferredSize = new Dimension(gridSizeX * coordPerGridPos, gridSizeY * coordPerGridPos)
    override def paintComponent(g: Graphics2D): Unit =
      super.paintComponent(g) //TODO what
      g.drawImage(planesOnMap, 0, 0, null)

    //panel listens to mouse
    this.listenTo(mouse.moves)
    this.listenTo(mouse.clicks)

    this.reactions += {
      //records mosue moves and coordiantes to vars
      case event.MouseMoved(_, point: java.awt.Point, _) =>
        mouseX = point.x
        mouseY = point.y
      case event.MouseClicked(_, _, _, _, _) =>
        //reacts if mouse clicked
        val clickCoord = Coord(mouseX, mouseY)
        //gets posissible plane or runway
        val possiblePlane = game.planeAtCoord(clickCoord)
        val possibleRunway = game.runwayAtCoord(clickCoord)
        if possiblePlane.isEmpty && possibleRunway.isEmpty then
          //if both none, empty the selectedPlane
          selectedPlane = None
        else if possibleRunway.isEmpty then
          //if possiblePlane defined, select the plane
          possiblePlane.foreach( n => selectedPlane = Some(n))
        else if selectedPlane.forall( n => isFlyingAction(n.action.getClass.getTypeName) ) then //weakness
          //if possiblePlane is doing a flying action (eg. goingToRunway or Circling) and possibleRunway is defined, set the plane to go to the runway
          selectedPlane.foreach( _.action = GoingToRunway(selectedPlane.get, possibleRunway.get) )
        //put the info of selectedPlane into the planeInfo box (that gets put into the right sidebar). PlaneTextToDisplay handles None-case
        planeInfo.text = PlaneTextToDisplay(selectedPlane).text
    }
  end mapPanel

  //definitions for right-sidebar buttons, filtering for display done later
  val buttonDim = Dimension(30, 10) //TODO
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
  val cirleLeftButton = new Button("Circle left"):
    maximumSize = buttonDim
    action = new Action("Circle left"):
      def apply() = (selectedPlane.foreach( n => n.action = CirclingLeft(n) ))
  val cirleRightButton = new Button("Circle right"):
    maximumSize = buttonDim
    action = new Action("Circle right"):
      def apply() = (selectedPlane.foreach( n => n.action = CirclingRight(n) ))
  val expediteButton = new Button("Expedite off runway"):
    maximumSize = buttonDim
    action = new Action("Expedite off runway"):
      def apply() = (selectedPlane.foreach( n => n.action = Expediting(n) ))
  def toRunwayButton(runway: Runway) = new Button("Taxi to runway #" + runway.index):
    maximumSize = buttonDim
    action = new Action("Taxi to runway #" + runway.index):
      def apply() = (selectedPlane.foreach( n => n.action = TaxiingToRunway(n, runway) ))
  val takeOffButton = new Button("Take off"):
    maximumSize = buttonDim
    action = new Action("Take off"):
      def apply() =
        (selectedPlane.foreach( n => n.action = TakingOff(n, game.grid.runways.filter( m => m.airplanesWaitingForTakeoff.contains(n) ).head) ))

  //textbox for righ-sidebar
  val planeInfo = new TextArea(15, 100):
    maximumSize = new Dimension(150, 300)
    editable = false
    wordWrap = true
    lineWrap = true

  //initial value
  planeInfo.text = PlaneTextToDisplay(None).text

  //right-sidebar
  var rightPanel = new BoxPanel(Orientation.Vertical):
    preferredSize = new Dimension(150, gridSizeY * coordPerGridPos)
    contents += planeInfo
  
  //vecort for actions that are considered flying
  val flyingActions: Vector[String] = Vector("GoingToRunway", "CirclingLeft", "CirclingRight")

  //helper functions for checking states of the aricraft
  def isFlyingAction(actionName: String): Boolean =
    flyingActions.exists(n => actionName.contains( n ) )
  def isLanding(plane: Airplane): Boolean =
    plane.action.getClass.getTypeName.contains("Landing")
  def isBoarded(plane: Airplane): Boolean =
    plane.action.getClass.getTypeName.contains("Boarded")
  def waitingForTakeoff(plane: Airplane): Boolean =
    plane.action.getClass.getTypeName.contains("WaitingOnRunway")


  //main function of the right-sidebar. Redraws the available buttons based on the state of the selectedPlane
  def updateRightPanel() =
    rightPanel.contents.clear()
    rightPanel.contents += planeInfo
    if selectedPlane.isDefined then
      val plane = selectedPlane.get
      if isFlyingAction(plane.action.getClass.getTypeName) then
        rightPanel.contents += new BorderPanel:
          add(slowButton, BorderPanel.Position.West)
        rightPanel.contents += Swing.VStrut(10)
        rightPanel.contents += new BorderPanel:
          add(cruiseButton, BorderPanel.Position.West)
        rightPanel.contents += Swing.VStrut(10)
        rightPanel.contents += new BorderPanel:
          add(fastButton, BorderPanel.Position.West)
        rightPanel.contents += Swing.VStrut(20)
        rightPanel.contents += new BorderPanel:
          add(cirleLeftButton, BorderPanel.Position.West)
        rightPanel.contents += Swing.VStrut(10)
        rightPanel.contents += new BorderPanel:
          add(cirleRightButton, BorderPanel.Position.West)
        rightPanel.contents += Swing.VStrut(10)
      if isLanding(plane) then
        rightPanel.contents += new BorderPanel:
          add(expediteButton, BorderPanel.Position.West)
        rightPanel.contents += Swing.VStrut(10)
      if isBoarded(plane) then
        game.grid.runways.foreach( runway => {
          rightPanel.contents += new BorderPanel:
            add(toRunwayButton(runway), BorderPanel.Position.West)
          rightPanel.contents += Swing.VStrut(10) })
      if waitingForTakeoff(plane) then
        rightPanel.contents += new BorderPanel:
          add(takeOffButton, BorderPanel.Position.West)
        rightPanel.contents += Swing.VStrut(10)
  end updateRightPanel

  //textbox for left-sidebar
  val arrivingInfo = new TextArea(15, 100):
    maximumSize = new Dimension(150, 300)
    editable = false
    wordWrap = true
    lineWrap = true

  //initial value
  arrivingInfo.text = game.arrivingMessages.mkString("\n")

  //textbox for left-sidebar
  val airportInfo = new TextArea(15, 100):
    maximumSize = new Dimension(150, 300)
    editable = false
    wordWrap = true
    lineWrap = true

  //initial value
  airportInfo.text = AirportInfo(game).text

  //function for updating left sidebar
  def updateLeftPanel() =
    arrivingInfo.text = game.arrivingMessages.mkString("\n")
    airportInfo.text = AirportInfo(game).text

  //left-sidebar
  var leftPanel = new BoxPanel(Orientation.Vertical):
    preferredSize = new Dimension(150, gridSizeY * coordPerGridPos)
    contents += arrivingInfo
    contents += Swing.VStrut(10)
    contents += airportInfo

  //the panel combining all teh pervious panels into final version
  val combinationPanel = new BoxPanel(Orientation.Horizontal):
    contents += leftPanel
    contents += mapPanel
    contents += rightPanel


  //called by top when time advances
  def tick(): Unit =
    game.tick()
    //light multithreading for efficiency
    Future {
      drawPlanes()
      updateRightPanel() }
      .onComplete { _ =>
        Swing.onEDT {
          mapPanel.repaint()
          planeInfo.text = PlaneTextToDisplay(selectedPlane).text }}

  //called more seldomly, as the info is not so critical
  def slowerTick(): Unit =
    Future {
      updateLeftPanel() }
      .onComplete { _ =>
        Swing.onEDT {
          //leftPanel.repaint()
          }}

  //main (and only) window
  def top = new MainFrame:
    title = "Air Traffic Controller"

    //timers
    javax.swing.Timer(200, e => tick()).start()
    javax.swing.Timer(1000, e => slowerTick()).start()

    contents = combinationPanel