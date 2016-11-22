package scalar
import scala.collection.mutable.ArrayBuffer

class ScalaR {
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

	def c(values: Any*): RVector[Type] = {
		return new RVector[Integer](new ArrayBuffer[Integer]())
	}

	def getVariableMappings = variableMappings
}