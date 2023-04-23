package airplaneGame

class Degrees(val value: Int):

  def +(howMuch: Int): Degrees = Degrees(math.abs((value + howMuch) % 360))
  def -(howMuch: Int): Degrees =
    if value < howMuch then
      Degrees(360 + value - howMuch)
    else
      Degrees(value - howMuch)

  override def toString: String = s"$value"