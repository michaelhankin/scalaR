package scalar
import scala.collection.mutable._
import TypeUtils._
import DataFrameUtils._
import VectorUtils._
import com.quantifind.charts.Highcharts._

// TODO: Make implicit Int / Double / Boolean / String to RVector
// eg 'v = 1
//    'v == 1 etc

object ScalaR {
	def NA = new NAType
	var variableMappings: Map[Symbol, RVector] = Map[Symbol, RVector]()
	var dfMappings: Map[Symbol, DataFrame] = Map[Symbol, DataFrame]()

	implicit class VariableWrapper(s: Symbol) {

		def apply(idx: Int): RVector = {
			val result = if (variableMappings.contains(s)) variableMappings(s)(idx)
						 else dfMappings(s)(idx)
			result
		}

		def apply(r: Range): RVector = {
			val vec = variableMappings(s)
			return vec(r.min - 1, r.max)
		}

		def apply(rowIdx: Int, colIdx: Int): RVector = {
			val df = dfMappings(s)
			if (colIdx == -1) {
				val currRow = df.getRow(rowIdx)
				new RVector(currRow, currRow(0).getType)
			}
			df(rowIdx, colIdx)
		}	

		def apply(col: String): RVector = {
			val df = dfMappings(s)
			df(col)
		}

		def apply(cols: RVector): DataFrame = {
			val df = dfMappings(s)
			df(cols)
		}

		def apply(sym: Symbol): Any = {
			val df = dfMappings(s)
			val value = variableMappings(sym)
			df(value)
		}

		def <--(value: Any) = {
			val buf = ArrayBuffer[Type]()
			value match {
				case b: Boolean  => buf += new Logical(b)
				var vec = new RVector(buf, "Logical")
				variableMappings += (s -> vec)
				case i: Int      => buf += new Numeric(i)
				var vec = new RVector(buf, "Numeric")
				variableMappings += (s -> vec)
				case d: Double   => buf += new Numeric(d)
				var vec = new RVector(buf, "Numeric")
				variableMappings += (s -> vec)
				case str: String => buf += new Character(str)
								    var vec = new RVector(buf, "Character")
									variableMappings += (s -> vec)
				case na: NAType => buf += new NAType
								   var vec = new RVector(buf, "Logical")
								   variableMappings += (s -> vec)
				case v: RVector  => variableMappings += (s -> v)	
				case df: DataFrame => dfMappings += (s -> df)
			}
		}

		def <--(variable: Symbol) = {
			if (variableMappings.contains(variable)) {
				variableMappings += (s -> variableMappings(variable))
			} else {
				val name = variable.name
				throw new RuntimeException(s"Error: object '$name' not found")
			}
		}

		def +(that: Symbol): RVector = {
			return variableMappings(s) + variableMappings(that)
		}

		def -(that: Symbol): RVector = {
			return variableMappings(s) - variableMappings(that)
		}

		def ==(other: Symbol): Boolean = {
			val a = variableMappings(s)
			val b = variableMappings(other)
			return a.getType == b.getType && a.data == b.data
		}
	}

	def c(values: Any*): RVector = {
		val typeHierarchy = Array("Logical", "Integer", "Numeric", "Character")
		var highestType: String = "Logical"

		for (v <- values) {
			val curType = v match {
				case n: NAType  => "NA"
				case b: Boolean => "Logical"
				case i: Int     => "Numeric"
				case d: Double  => "Numeric"
				case s: String  => "Character"
				case _          => "Unsupported Type"
			}

			if (curType == "Unsupported Type")
			throw new IllegalArgumentException(s"${v.toString} has unsupported type")

			val curIdx = typeHierarchy.indexOf(curType)
			if (curIdx > typeHierarchy.indexOf(highestType)) {
				highestType = curType
			}
		}

		var buf = ArrayBuffer[Type]()
		return highestType match {
			case "Logical" => new RVector(buf ++ values.map(toLogical), "Logical")
			case "Numeric" => new RVector(buf ++ values.map(toNumeric), "Numeric")
			case "Character" => new RVector(buf ++ values.map(toCharacter), "Character")
		}
	}

	def data_frame(cols: RVector*): DataFrame = {
		val data = cols.to[ArrayBuffer]
		if (!data.forall(col => col.length == data(0).length))
			throw new RuntimeException("Error: all input vectors must have the same length")
		var schema = LinkedHashMap[String, (Int, String)]()
		for ((col, i) <- data.zipWithIndex) {
			schema += (s"v${i + 1}" -> (i + 1, col.getType))
		}
		new DataFrame(data, schema)
	}

	def data_frame_from_vars(cols: Symbol*): DataFrame = {
		val data = cols.map(variableMappings(_))
		data_frame(data: _*)
	}

	// // Create DataFrame from list of vectors, with default column names set
	// def data_frame(cols: Any*): DataFrame = {
	// 	val colClass = cols(0).getClass.toString
	// 	// println(colClass)
	// 	val data = ArrayBuffer[RVector]()
	// 	if (colClass == "class scalar.RVector") {
	// 		val data = cols.map(_.asInstanceOf[RVector]).to[ArrayBuffer]
	// 	} else {
	// 		val data = cols.map(_.asInstanceOf[Symbol]).map(variableMappings(_)).to[ArrayBuffer]
	// 	}
	// 	var schema = Map[String, (Int, String)]()
	// 	for ((col, i) <- data.zipWithIndex) {
	// 		schema += (s"v${i + 1}" -> (i + 1, col.getType))
	// 	}

	// 	new DataFrame(data, schema)
	// }

	// def data_frame_with_schema(schema: Any, cols: Any*): DataFrame = {
	// 	val schemaClass = schema.getClass.toString
	// 	val colClass = cols(0).getClass.toString
	// 	if (schemaClass == "class scalar.RVector") {
	// 		if (colClass == "class scalar.RVector") {
	// 			data_frame_with_schema_from_vecs(schema.asInstanceOf[RVector], cols.map(_.asInstanceOf[RVector]): _*)
	// 		} else {
	// 			data_frame_with_schema_from_vecsyms(schema.asInstanceOf[RVector], cols.map(_.asInstanceOf[Symbol]): _*)
	// 		}
	// 	} else {
	// 		if (colClass == "class scalar.RVector") {
	// 			data_frame_with_schema_from_symvecs(schema.asInstanceOf[Symbol], cols.map(_.asInstanceOf[RVector]): _*)
	// 		} else {
	// 			data_frame_with_schema_from_syms(schema.asInstanceOf[Symbol], cols.map(_.asInstanceOf[Symbol]): _*)
	// 		}
	// 	}
	// }

	// def data_frame_with_schema_from_vecs(schemaVec: RVector, cols: RVector*): DataFrame = {
	// 	if (schemaVec.getType != "Character") 
	// 		throw new RuntimeException("Error: schema vector must contain only character strings")
	// 	val data = ArrayBuffer[RVector](cols: _*)
	// 	val schemaArr = unpackCharacterVector(schemaVec)
	// 	if (schemaArr.length != data.length)
	// 		throw new RuntimeException("Error: schema vector must have length equal to the number of input column vectors")
	// 	var schema = Map[String, (Int, String)]()
	// 	for ((colName, (colVec, i)) <- schemaArr zip data.zipWithIndex) {
	// 		schema += (colName -> (i + 1, colVec.getType))
	// 	}
	// 	new DataFrame(data, schema)
	// }

	// def data_frame_with_schema_from_symvecs(schemaSym: Symbol, cols: RVector*): DataFrame = {
	// 	val unwrappedSv = variableMappings(schemaSym)
	// 	data_frame_with_schema(unwrappedSv, cols: _*)
	// }

	// def data_frame_with_schema_from_vecsyms(schemaVec: RVector, cols: Symbol*): DataFrame = {
	// 	val colArr = cols.map(variableMappings(_))
	// 	data_frame_with_schema(schemaVec, colArr: _*)
	// }

	// def data_frame_with_schema_from_syms(schemaSym: Symbol, cols: Symbol*): DataFrame = {
	// 	val colArr = cols.map(variableMappings(_))
	// 	val schema = variableMappings(schemaSym)
	// 	data_frame_with_schema(schema, colArr: _*)
	// }

	def read_csv(path: String, header: Boolean = true, delim: String = ",", naString: String = ""): DataFrame = {
		val data = CsvParser.read_data(path, header, delim, naString)
		new DataFrame(data._1, data._2)
	}

	def print(s: Symbol) = {

		var foundVec = false

		val vec = variableMappings.get(s)
		vec match {
  			case Some(value) => println(value); foundVec = true
  			case None => foundVec = false
		}

		if (!foundVec) {
			val df = dfMappings.get(s)
			df match {
				case Some(value) => value.printdf()
				case None => throw new Exception(s"object '${s}' not found")
			}
		}
	}

	def print(vec: RVector): Unit = println(vec)

	def print(df: DataFrame): Unit = df.printdf()

	// basic R usage functions
	def length(s: Symbol): RVector = length(variableMappings(s))

	def typeOf(s: Symbol) = println(variableMappings(s).getType)

	def mean(s: Symbol): RVector = {
		val vec = variableMappings(s)
		return mean(vec)
	}

	def sd(s: Symbol): RVector = {
		val vec = variableMappings(s)
		return sd(vec)
	}

	def typeOf(vec: RVector)  = println(vec.getType)

	def sum(s: Symbol): RVector = {
		val vec = variableMappings(s)
		return sum(vec)
	}

	def plotlm(x: RVector, y: RVector, main: String = "", xlab: String = "", ylab: String = "") = {
		regression((unpackNumericVector(x), unpackNumericVector(y)))
		title(main)
		xAxis(xlab)
		yAxis(ylab)
	}

	def setdiff(s0: Symbol, vec1: RVector): RVector = {
		val vec0 = variableMappings(s0)
		setdiff(vec0, vec1)
	}

	def setdiff(vec0: RVector, s1: Symbol): RVector = {
		val vec1 = variableMappings(s1)
		setdiff(vec0, vec1)
	}

	def setdiff(s0: Symbol, s1: Symbol): RVector = {
		val vec0 = variableMappings(s0)
		val vec1 = variableMappings(s1)
		setdiff(vec0, vec1)
	}

	def plot(x: RVector, y: RVector, main: String = "", xlab: String = "", ylab: String = "") = {
		scatter((unpackNumericVector(x), unpackNumericVector(y)))
		title(main)
		xAxis(xlab)
		yAxis(ylab)
	}

	def hist(x: RVector, bins: Int, main: String = "", xlab: String = "", ylab: String = "") = {
		histogram(unpackNumericVector(x), bins)
		title(main)
		xAxis(xlab)
		yAxis(ylab)
	}

	def length(vec: RVector): RVector = new RVector(ArrayBuffer[Type]() += new Numeric(vec.length), "Numeric")

	def mean(vec: RVector): RVector = {
		var numvec: RVector = null
		if (vec.getType == "Logical" || vec.getType == "Numeric"){
			numvec = asNumeric(vec)
		}
		else if (vec.getType == "Character")
		throw new IllegalArgumentException("Argument is not Numeric")

		var sum = 0.0
		for (v <- numvec.data) {
			if (v.storedValue == "NA")
			return new RVector(ArrayBuffer[Type](new NAType), "Logical")
			else
			v.storedValue match {
				case d: Double => sum += d
			}
		}
		val mu = sum / numvec.length
		return new RVector(ArrayBuffer[Type](new Numeric(mu)), "Numeric")
	}

	def sd(vec: RVector): RVector = {
		var numvec: RVector = null
		if (vec.getType == "Logical" || vec.getType == "Numeric") {
			numvec = asNumeric(vec)
		}
		else if (vec.getType == "Character")
		throw new IllegalArgumentException("Argument is not Numeric")

		val xbar: Double = mean(numvec)(1).data(0).storedValue match {
			case d: Double => d
		}

		var sumsq = 0.0
		for (v <- numvec.data) {
			if (v.storedValue == "NA")
			return new RVector(ArrayBuffer[Type](new NAType), "Logical")
			else
			v.storedValue match {
				case d: Double => sumsq += math.pow((d-xbar),2)
			}
		}
		val sigma = math.sqrt(sumsq / (numvec.length - 1))
		return new RVector(ArrayBuffer[Type](new Numeric(sigma)), "Numeric")
	}

	def sum(vec: RVector): RVector = {
		var numvec: RVector = null
		if (vec.getType == "Logical" || vec.getType == "Numeric"){
			numvec = asNumeric(vec)
		}
		else if (vec.getType == "Character")
		throw new IllegalArgumentException("Argument is not Numeric")

		var sum = 0.0
		for (v <- numvec.data) {
			if (v.storedValue == "NA")
			return new RVector(ArrayBuffer[Type](new NAType), "Logical")
			else
			v.storedValue match {
				case d: Double => sum += d
			}
		}
		return new RVector(ArrayBuffer[Type](new Numeric(sum)), "Numeric")
	}

	def asLogical(vec: RVector): RVector = {
		var buf = ArrayBuffer[Type]() ++ vec.data.map(toLogical)
		return new RVector(buf, "Logical")
	}

	def asNumeric(vec: RVector): RVector = {
		var buf = ArrayBuffer[Type]() ++ vec.data.map(toNumeric)
		return new RVector(buf, "Numeric")
	}
	def asCharacter(vec: RVector): RVector = {
		var buf = ArrayBuffer[Type]() ++ vec.data.map(toCharacter)
		return new RVector(buf, "Character")
	}

	def setdiff(vec0: RVector, vec1: RVector): RVector = {
		var result = null
		if (vec0.getType != vec1.getType) {
			throw new RuntimeException("Error: input vectors have conflicting types")
		} else {
			var result = new RVector(vec0.data.diff(vec1.data), vec0.getType)
		}
		result
	}

	def head(s: Symbol): Unit = {
		head(dfMappings(s), 5)
	}

	def head(df: DataFrame, count: Int = 5) = {
		df.head(count + 1)
	}

	def subset(s: Symbol, formula: String, select: String = ""): DataFrame = {
		subset(dfMappings(s), formula, select)
	}

	def subset(df: DataFrame, formula: String, select: String): DataFrame = {
		var columns = select.split(" ").map(_.trim)
		var colIndexes = new ArrayBuffer[Int]()
		var schema_map = LinkedHashMap[String, (Int, String)]()
		var p = 0
		for((k,v) <- df.schema) {
			if (columns.contains(k) || select.equals("")) {
				schema_map(k) = (p, v._2)
				colIndexes += df.schema(k)._1
				p = p + 1
			}
		}

		var args = formula.split(" ").map(_.trim)
		var col = args(0)
		var operator = args(1)
		var num = args(2)
		var rows = new ArrayBuffer[Int]()

		val subset_col = df(col).data.zipWithIndex

		for ((v,i) <- subset_col) {
			operator match {
				case ">" => {
					val v2 = v.storedValue match {
						case d: Double => d
					}

					if (v2 > num.toDouble){
						rows += i
					}
				}
				case "<" => {
					val v2 = v.storedValue match {
						case d: Double => d
					}

					if (v2 < num.toDouble) {
						rows += i
					}
				}
				case "<=" => {
					val v2 = v.storedValue match {
						case d: Double => d
					}

					if (v2 <= num.toDouble)
						rows += i
				}
				case ">=" => {
					val v2 = v.storedValue match {
						case d: Double => d
					}

					if (v2 >= num.toDouble)
						rows += i
				}
				case "==" => {
					val v2 = v.storedValue match {
						case d: Double => d
					}

					if (v2 == num.toDouble)
						rows += i
				}
			}
		}

		var bigBuf = ArrayBuffer[ArrayBuffer[Type]]()
		for (_ <- 0 until colIndexes.length) {
			bigBuf += new ArrayBuffer[Type]()
		}

		for (i <- rows) {
			val row = df.getRow(i+1)
			for ((j,k) <- colIndexes.zipWithIndex) {
				bigBuf(k) += row(j)
			}
		}

	    var retBuf = ArrayBuffer[RVector]()
	    var j = 0
	    for(buff <- bigBuf) {
	      var vec = new RVector(buff, "Numeric")
	      retBuf += vec
	      j = j + 1
		}

		return new DataFrame(retBuf, schema_map)
	}
}
