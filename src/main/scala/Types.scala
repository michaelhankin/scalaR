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

object TypeUtils {
	def asLogical(x: Boolean): Logical = Logical(x)

	def asInteger(x: Boolean): Integer = {
		val value = if (x) 1 else 0
		Integer(value)
	}

	def asNumeric(x: Any): Numeric = {
		val value = x.getClass match {
			case java.lang.Integer => x.toDouble
			case java.lang.Boolean => if (x) 1.0 else 0.0
			case _ => throw new RuntimeException("Error: argument has invalid type")
		}
		Numeric(value)
	}

	def asCharacter(x: Any): Character = {
		val value = x.getClass match {
			case java.lang.Double => x.toString
			case java.lang.Integer => x.toString
			case java.lang.Boolean => x.toString
			case _ => throw new RuntimeException("Error: argument has invalid type")
		}
		Character(value)
	}
}
