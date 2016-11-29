package scalar
import scala.collection.mutable.ArrayBuffer
import TypeUtils._
import DataFrameUtils._
import VectorUtils._


// TODO: Make implicit Int / Double / Boolean / String to RVector 
// eg 'v = 1
//    'v == 1 etc 

class ScalaR {
	def NA = new NAType
	var variableMappings: Map[Symbol, RVector] = Map[Symbol, RVector]()

	implicit class VariableWrapper(s: Symbol) {
		def apply(idx: Int): Any = {
			val vec = variableMappings(s)
			return vec(idx).storedValue
		}

		def apply(r: Range): RVector = {
			val vec = variableMappings(s)
			return vec(r.min - 1, r.max)
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
				case v: RVector  => variableMappings += (s -> v)	
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

		// def ==(value: Type) = {
		// 	if (variableMappings.contains(s) && variableMappings(s).getType == value.getType) {
		// 		value.storedValue == variableMappings(s).storedValue
		// 	} else if (!variableMappings.contains(s)) {
		// 		val name = s.name
		// 		throw new RuntimeException(s"Error: object '$name' not found")
		// 	} else {
		// 		throw new RuntimeException("Error: input objects must have same type")
		// 	}
		// }

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

	def print(s: Symbol) = {

	}

	// Construct a DataFrame object from a sequence of vectors
	// def data_frame(values: Any*): DataFrame = {

	// }

	// basic R usage functions 
	def length(s: Symbol): Int = variableMappings(s).length

	def typeOf(s: Symbol): String = variableMappings(s).getType

	def mean(s: Symbol): RVector = {
		val vec = variableMappings(s)
		return mean(vec)
	}

	def sd(s: Symbol): RVector = {
		val vec = variableMappings(s)
		return sd(vec)
	}

		def length(vec: RVector): Int = vec.length

	def mean(vec: RVector): RVector = {
		var numvec: RVector = null
		if (vec.getType == "Logical" || vec.getType == "Numeric"){
			numvec = asNumeric(vec)
		}
		else if (vec.getType == "Charcter")
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
		else if (vec.getType == "Charcter")
			throw new IllegalArgumentException("Argument is not Numeric")

		val xbar: Double = mean(numvec)(1).storedValue match {
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
	


	// def asLogical(vec: RVector): RVector = {
	// 	var buf = ArrayBuffer[Type]() ++ vec.data.map(toLogical)
	// 	return new RVector(buf, "Logical")
	// }

	// def asNumeric(vec: RVector): RVector = {
	// 	var buf = ArrayBuffer[Type]() ++ vec.data.map(toNumeric)
	// 	return new RVector(buf, "Numeric")
	// }
	// def asCharacter(vec: RVector): RVector = {
	// 	var buf = ArrayBuffer[Type]() ++ vec.data.map(toCharacter)
	// 	return new RVector(buf, "Character")
	// }

	def getVariableMappings = variableMappings
}