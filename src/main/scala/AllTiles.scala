object AllTiles:
  
  val allStrings = List("00000000", "00010000", "00010001", "00000001")  //for testing
  
  def ascii(str: String): String =
    str match
      case "00000000" =>
        "ground      "
      case "00010000" =>
        "Runway start"
      case "00010001" =>
        "Runway      "
      case "00000001" =>
        "Runway end  "
      case "nothing" =>
        "nothing"


  //TODO reading file names from source directory