abstract class Type(value: Any) {
	var storedValue: Any
	def getType(): String
} 

class Logical(value: Boolean) extends Type {
	var storedValue: Boolean = value
	def getType(): String = "Logical"
}

class Numeric(value: Double) extends Type {
	var storedValue: Double = value
	def getType(): String = "Numeric"
}

class Integer(value: Int) extends Type {
	var storedValue: Int = value
	def getType(): String = "Integer"
}

class Character(value: String) extends Type {
	var storedValue: String = value
	def getType(): String = "Character"
}
