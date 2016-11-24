package scalar
import scala.collection.mutable.ArrayBuffer
import TypeUtils._

class ScalaR {
	def NA = new NAType
	var variableMappings: Map[Symbol, RVector[Type]] = Map[Symbol, RVector[Type]]()

	implicit class VariableWrapper(s: Symbol) {

		def apply(idx: Int): Any = {
			val vec = variableMappings(s)
			return vec(idx-1).storedValue
		}

		def <--(value: Boolean) = {
			val buf = ArrayBuffer[Logical]()
			buf += new Logical(value)
			var vec = new RVector[Logical](buf)
			variableMappings += (s -> vec)
		}

		def <--(value: Int) = {
			val buf = ArrayBuffer[Integer]()
			buf += new Integer(value)
			var vec = new RVector[Integer](buf)
			variableMappings += (s -> vec)
		}

		def <--(value: Double) = {
			val buf = ArrayBuffer[Numeric]()
			buf += new Numeric(value)
			var vec = new RVector[Numeric](buf)
			variableMappings += (s -> vec)
		}

		def <--(value: String) = {
			val buf = ArrayBuffer[Character]()
			buf += new Character(value)
			var vec = new RVector[Character](buf)
			variableMappings += (s -> vec)
		}

		def <--(vector: RVector[Type]) = {

		}

		def <--(variable: Symbol) = { 
			if (variableMappings.contains(variable)) {
				variableMappings += (s -> variableMappings(variable))
			} else {
				val name = variable.name
				throw new RuntimeException(s"Error: object '$name' not found")
			}
		}

		def getType(): String = {
			val vec = variableMappings(s)
			return vec.getType()
		}

		def length: Int = variableMappings(s).length

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

		// def ==(value: Symbol) = {
		// 	if (variableMappings.contains(s) && variableMappings.contains(value) && variableMappings(s).getType == variableMappings(value).getType) {
		// 		variableMappings(value).storedValue == variableMappings(s).storedValue
		// 	} else if (!variableMappings.contains(s)) {
		// 		val name = s.name
		// 		throw new RuntimeException(s"Error: object '$name' not found")
		// 	} else if (!variableMappings.contains(value)) {
		// 		val name = value.name
		// 		throw new RuntimeException(s"Error: object '$name' not found")
		// 	} else {
		// 		throw new RuntimeException("Error: input objects must have same type")
		// 	}
		// }
	}

	def c(values: Any*): Either[RVector[Logical], Either[RVector[Integer], 
	Either[RVector[Numeric], RVector[Character]]]] = {
		// Logical: Left(RVec[Logical])
		// Integer: Right(Left(RVec[Integer]))
		// Numeric: Right(Right(Left(RVec[Numeric])))
		// Character: Right(Right(Right(RVec[Character])))

		val typeHierarchy = Array("Logical", "Integer", "Numeric", "Character")
		var highestType: String = "Logical"

		for (v <- values) {
			val curType = v match {
				case n: NAType  => "NA"
				case b: Boolean => "Logical"
				case i: Int     => "Integer"
				case d: Double  => "Numeric"
				case s: String  => "String"
				case _          => "Unsupported Type"
			}

			if (curType == "Unsupported Type") 
				throw new IllegalArgumentException(s"Unsupported type: ${v.toString}")
			
			val curIdx = typeHierarchy.indexOf(curType)
			if (curIdx > typeHierarchy.indexOf(highestType)) {
				highestType = curType
			}
		}

		return highestType match {
			case "Logical" => Left(new RVector[Logical](ArrayBuffer[Logical]() ++ values.map(asLogical)))
			case "Integer" => Right(Left(new RVector[Integer](ArrayBuffer[Integer]() ++ values.map(asInteger))))
			case "Numeric" => Right(Right(Left(new RVector[Numeric](ArrayBuffer[Numeric]() ++ values.map(asNumeric)))))
			case "Charatcer" => Right(Right(Right(new RVector[Character](ArrayBuffer[Character]() ++ values.map(asCharacter)))))
		}

		// for (v <- values) {
		// 	if (highestType == "Logical")
		// 		buf += new Logical(asLogical(v))
		// 	else if (highest == "Integer")
		// 		buf += new Integer(asInteger(v))
		// 	else if (highest == "Numeric")
		// 		buf += new Numeric(asNumeric(v))
		// 	else 
		// 		buf += new Character(asCharacter(v))
		// }

		// var vec = 
		// if (highestType == "Logical")
		// 	new RVector[Logical](buf)
		// else if (highest == "Integer")
		// 	new RVector[Integer](buf)
		// else if (highest == "Numeric")
		// 	new RVector[Numeric](buf)
		// else 
		// 	new RVector[Character](buf)
		// var vec = highestType match {
		// 	case "Logical" => new RVector[Logical](buf)
		// 	case "Integer" => new RVector[Integer](buf) 
		// 	case "Numeric" => new ArrayBuffer[Numeric](buf)
		// 	case "Charatcer" => new ArrayBuffer[Character](buf)
		// } 
	}

	// object c {
	// 	val typeHierarchy = Array("Logical", "Integer", "Numeric", "Character")

	// 	def apply(values: Any*): Vector = {
	// 		var highestType: String = "Logical" 
	// 		for (value <- values) {
	// 			val currType = value.getClass match {
	// 				case currType: java.lang.Integer => "Integer"
	// 				case java.lang.String => "Character"
	// 				case java.lang.Double => "Numeric"
	// 				case java.lang.Boolean => "Logical"
	// 				case _ => "Unknown"
	// 			}
	// 			val currIdx = typeHierarchy.indexOf(currType)
	// 			if (currIdx != -1) {
	// 				if (typeHierarchy.indexOf(currType) > typeHierarchy.indexOf(highestType)) {
	// 					highestType = currType
	// 				}
	// 			} else {
	// 				throw new RuntimeException(s"Error: element at index $currIdx has invalid type")
	// 			}
	// 		}
	// 		var vecArr: ArrayBuffer[Type] = highestType match {
	// 			case "Integer" => values.map(asInteger).to[ArrayBuffer]
	// 			case "Character" => values.map(asCharacter).to[ArrayBuffer]
	// 			case "Numeric" => values.map(asNumeric).to[ArrayBuffer]
	// 			case "Logical" => values.map(asLogical).to[ArrayBuffer]
	// 		}
	// 		Vector(vecArr)
	// 	}
	// }

	def getVariableMappings = variableMappings
}