package scalar
import scala.util.Try
import scala.collection.mutable._

object CsvParser {
  var path: String = _

  def read_data(path: String, headers: Boolean, delim: String, naString: String = ""): 
  (ArrayBuffer[RVector], Map[String, (Int,String)]) = {
      val bufferedSource = io.Source.fromFile(path)
      var lines = bufferedSource.getLines.toList
      val headers = lines(0).split(delim).map(_.trim)
      var first_data_row = lines(1)

    var bigBuf = ArrayBuffer[ArrayBuffer[Type]]()
    var typeBuf = ArrayBuffer[String]()


    for(row <- first_data_row.split(delim).map(_.trim)) {
      if(!row.isEmpty) {
        var col_type = infer_type(row)
        typeBuf += col_type
        var buf = ArrayBuffer[Type]()
        bigBuf += buf
      }
    }

    for (i <- 1 until lines.length) {
      val cols = lines(i).split(delim).map(_.trim)
      for(j <- 0 until cols.length){
        val rtype = typeBuf(j)


        if (cols(j).toLowerCase == naString) {
          bigBuf(j) += new NAType
        } else {

          rtype match {
            case "Logical" => bigBuf(j) += new Logical(cols(j).toBoolean)
            case "Numeric" => bigBuf(j) += new Numeric(cols(j).toDouble)
            case "Character"   => bigBuf(j) += new Character(cols(j).toString)
          }
        }
      }
    }

    var retBuf = ArrayBuffer[RVector]()
    var j = 0
    for(buff <- bigBuf){
      var vec = new RVector(buff, typeBuf(j))
      retBuf += vec
      j = j + 1
    }

    var schema_map = Map[String, (Int, String)]()
    var p = 0
    for(row <- headers){
      schema_map(row) = (p, typeBuf(p))
      p = p + 1
    }

    bufferedSource.close

    (retBuf, schema_map)
  }

  def infer_type(input: String): String = {

    if(Try{input.toDouble}.isSuccess){
      return "Numeric"
    }

    if(input.toLowerCase() == "true" || input.toLowerCase() == "false"){
      return "Logical"
    }

    return "Character"

  }

}
