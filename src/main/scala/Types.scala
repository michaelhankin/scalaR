package ut.cs.cs345.scalar

abstract class Type {
	type T >: Any
	def getType: String
	def storedValue: T
} 

class Logical(value: Boolean) extends Type {
	def storedValue: Boolean = value
	def getType(): String = "Logical"
}

class Numeric(value: Double) extends Type {
	def storedValue: Double = value
	def getType(): String = "Numeric"
}

class Integer(value: Int) extends Type {
	def storedValue: Int = value
	def getType(): String = "Integer"
}

class Character(value: String) extends Type {
	def storedValue: String = value
	def getType(): String = "Character"
}
