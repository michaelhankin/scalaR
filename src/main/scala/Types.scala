package scalar

abstract class Type {
	type T 
	def getType: String
	def storedValue: T
	override def toString: String = storedValue.toString
}

class Logical(value: Boolean) extends Type {
	type T = Boolean
	def storedValue: Boolean = value
	def getType: String = "Logical"
}

class Numeric(value: Double) extends Type {
	type T = Double
	def storedValue: Double = value
	def getType: String = "Numeric"

	def ==(that: Numeric): Boolean = {
		return this.storedValue == that.storedValue
	}
}

class Character(value: String) extends Type {
	type T = String
	def storedValue: String = value
	def getType: String = "Character"
}

class NAType extends Type {
	type T = String
	var curType: String = "Logical"
	def storedValue: String = "NA"  
	def getType: String = return curType
	def setType(t: String) = t match {
		case "Logical" => curType = "Logical"
		case "Numeric" => curType = "Numeric"
		case "Character" => curType = "Character"
	} 
	override def toString: String = "NA"
}


object TypeUtils {
	def toLogical(x: Any): Type = x match {
		case l: Logical => l
		case b: Boolean => new Logical(b)
		case int: Int => if (int == 1) new Logical(true) else new Logical(false)
		case n: Numeric => if (n.storedValue == 1.0) new Logical(true) else new Logical(false)
		case d: Double => if (d == 1.0) new Logical(true) else new Logical(false)
		case s: String => if (s == "true") new Logical(true)
						  else if (s == "false") new Logical(false)
						  else throw new IllegalArgumentException(s"${s} cannot be converted to Type Logical")
		case na: NAType => na.setType("Logical"); na
		case _ => throw new IllegalArgumentException(s"Unsupported type: ${x.toString}")
	}

	def toNumeric(x: Any): Type = x match {
		case l: Logical => if (l.storedValue) new Numeric(1) else new Numeric(0)
		case b: Boolean => if (b) new Numeric(1) else new Numeric(0)
		case i: Int => new Numeric(i)
		case n: Numeric => n
		case d: Double => new Numeric(d)
		case s: String =>
		try { 
			return new Numeric(s.toDouble)
		} catch {
			case e: Exception => throw new IllegalArgumentException(s"${s} cannot be converted to type numeric")
		}
		case na: NAType => na.setType("Numeric"); na
		case _ => throw new IllegalArgumentException(s"Unsupported type: ${x.toString}")
	}

	def toCharacter(x: Any): Type = x match {
		case na: NAType => na.setType("Character"); na
		case _ => new Character(x.toString)
	}
}
