package airplaneGame

import java.awt.Image
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon

object AllTiles: //depreceated
  
  def readScaledImage(filename: String): ImageIcon =
    ImageIcon(ImageIO.read(File("./Tiles/" + filename)).getScaledInstance(AirplaneGame.coordPerGridPos, AirplaneGame.coordPerGridPos, Image.SCALE_DEFAULT))
    
  val filenames = File("./Tiles").list
    
  def stripEnding(s: String) = s.takeWhile(_ != '.')
    
  lazy val allStrings    = filenames.map(stripEnding(_))
  lazy val images        = filenames.map(name => (stripEnding(name), readScaledImage(name))).toMap