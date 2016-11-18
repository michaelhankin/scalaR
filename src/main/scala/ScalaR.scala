package ut.cs.cs345.scalar
import scala.collection.mutable.ArrayBuffer

class ScalaR {
	var variableMappings: Map[Symbol, RVector[Type]] = Map[Symbol, RVector[Type]]()

	implicit class VariableWrapper(s: Symbol) {
		// def <--(value: Type) = {
		// 	variableMappings += (s -> value)
		// }
		def apply(idx: Int): Any = {
			val vec = variableMappings(s)
			return vec(idx-1).storedValue
		}

		def <--(value: Boolean) = {
			//val vec = new RVector()
			//variableMappings += (s -> new Logical(value))
		}

		def <--(value: Int) = {
			val buf = ArrayBuffer[Integer]()
			buf += new Integer(value)

			val vec = new RVector[Integer](buf)

			variableMappings += (s -> vec)
		}

		def <--(value: Double) = {
			// variableMappings += (s -> Logical(value))
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

	def getVariableMappings = variableMappings
}