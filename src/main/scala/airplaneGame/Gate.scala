package airplaneGame

//simple class for representing airport gates
class Gate(val index: Int, val loc: GridPos):
  
  //plane currently at or taxiing to gate
  var plane = Option.empty[Airplane]