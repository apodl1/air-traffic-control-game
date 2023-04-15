package airplaneGame

class Gate(index: Int, loc: GridPos):
  
  var plane = Option.empty[Airplane]