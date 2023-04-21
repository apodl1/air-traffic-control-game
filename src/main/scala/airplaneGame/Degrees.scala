package airplaneGame

class Degrees(val value: Int):

  def +(howMuch: Int): Degrees = Degrees(math.abs((value + howMuch) % 360))
  def -(howMuch: Int): Degrees = Degrees(math.abs((value - howMuch) % 360))

  override def toString: String = s"$value"