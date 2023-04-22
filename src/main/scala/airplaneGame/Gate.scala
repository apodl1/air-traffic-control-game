package airplaneGame

class Gate(index: Int, val loc: GridPos):
  
  var plane = Option.empty[Airplane]