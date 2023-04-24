package airplaneGame

class Gate(val index: Int, val loc: GridPos):
  
  var plane = Option.empty[Airplane]