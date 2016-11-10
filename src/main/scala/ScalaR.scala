object ScalaR {
	var variableMappings: Map[Symbol, Any] = _

	implicit class VariableWrapper(s: Symbol) {
		def <-(value: Type) {
			variableMappings += (s -> value)
		}

		def <-(variable: Symbol) {
			if (variableMappings.contains(variable)) {
				variableMappings += (s -> variableMappings(variable))
			} else {
				val name = variable.name
				throw new RuntimeException(s"Error: object '$name' not found")
			}
		}
	}
}