package ut.cs.cs345.scalar

object ScalaR {
	var variableMappings: Map[Symbol, Type] = Map[Symbol, Type]()

	implicit class VariableWrapper(s: Symbol) {
		// def <--(value: Type) = {
		// 	variableMappings += (s -> value)
		// }

		def <--(value: Boolean) = {
			variableMappings += (s -> Logical(value))
		}

		def <--(value: Boolean) = {
			variableMappings += (s -> Logical(value))
		}

		def <--(variable: Symbol) = {
			if (variableMappings.contains(variable)) {
				variableMappings += (s -> variableMappings(variable))
			} else {
				val name = variable.name
				throw new RuntimeException(s"Error: object '$name' not found")
			}
		}

		def ==(value: Type) = {
			if (variableMappings.contains(s) && variableMappings(s).getType == value.getType) {
				value.storedValue == variableMappings(s).storedValue
			} else if (!variableMappings.contains(s)) {
				val name = s.name
				throw new RuntimeException(s"Error: object '$name' not found")
			} else {
				throw new RuntimeException("Error: input objects must have same type")
			}
		}

		def ==(value: Symbol) = {
			if (variableMappings.contains(s) && variableMappings.contains(value) && variableMappings(s).getType == variableMappings(value).getType) {
				variableMappings(value).storedValue == variableMappings(s).storedValue
			} else if (!variableMappings.contains(s)) {
				val name = s.name
				throw new RuntimeException(s"Error: object '$name' not found")
			} else if (!variableMappings.contains(value)) {
				val name = value.name
				throw new RuntimeException(s"Error: object '$name' not found")
			} else {
				throw new RuntimeException("Error: input objects must have same type")
			}
		}
	}

	def getVariableMappings = variableMappings
}