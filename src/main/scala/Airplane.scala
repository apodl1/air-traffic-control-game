class Airplane(game: GameState, id: Int):

  var location: Coordinates = ???
  var bearing: Int = ???
  var speed: Int = ???
  var fuel: Double = ???

  val origin: String = ???
  var crashed: Boolean = false

  var action: Action = ???

  //TODO implimentetions and functions