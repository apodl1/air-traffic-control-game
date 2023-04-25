package airplaneGame

//supprting class for stating bearings, has rollover for going over or under 360, int value stored in val value
class Degrees(val value: Int):

  def +(howMuch: Int): Degrees = Degrees(math.abs((value + howMuch) % 360))
  def -(howMuch: Int): Degrees =
    if value < howMuch then
      Degrees(360 + value - howMuch)
    else
      Degrees(value - howMuch)

  override def toString: String = s"$value"