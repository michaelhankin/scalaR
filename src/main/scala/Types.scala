package scalar

abstract class Type {
	type T 
	def getType: String
	def storedValue: T
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
}

class Integer(value: Int) extends Type {
	type T = Int
	def storedValue: Int = value
	def getType: String = "Integer"
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
		case "Integer" => curType = "Integer"
		case "Character" => curType = "Character"
	} 
}


object TypeUtils {
	def asLogical(x: Any): Either[Logical, NAType] = x match {
		case l: Logical => Left(l)
		case b: Boolean => Left(new Logical(b))
		case i: Integer => if (i.storedValue == 1) Left(new Logical(true)) else Left(new Logical(false))
		case int: Int => if (int == 1) Left(new Logical(true)) else Left(new Logical(false))
		case n: Numeric => if (n.storedValue == 1.0) Left(new Logical(true)) else Left(new Logical(false))
		case d: Double => if (d == 1.0) Left(new Logical(true)) else Left(new Logical(false))
		case s: String => if (s == "true") Left(new Logical(true)) 
						 else if (s == "false") Left(new Logical(false)) 
						 else throw new IllegalArgumentException(s"${s} cannot be converted to Type Logical")
		case na: NAType => na.setType("Logical"); Right(na)
		case _ => throw new IllegalArgumentException(s"Unsupported type: ${x.toString}")
	}

	def asInteger(x: Any): Either[Integer, NAType] = x match {
		case l: Logical => if (l.storedValue) Left(new Integer(1)) else Left(new Integer(0))
		case b: Boolean => if (b) Left(new Integer(1)) else Left(new Integer(0))
		case i: Integer => Left(i)
		case int: Int => Left(new Integer(int))
		case n: Numeric => Left(new Integer(n.storedValue.toInt))
		case d: Double => Left(new Integer(d.toInt))
		case s: String => 
			try { 
				val strAsInt = s.toInt
				return Left(new Integer(strAsInt))
			} catch {
				case e: java.lang.NumberFormatException => 
					throw new IllegalArgumentException(s"${s} cannot be converted to Type Integer")
			}
		case na: NAType => na.setType("Integer"); Right(na)
		case _ => throw new IllegalArgumentException(s"Unsupported type: ${x.toString}")
	}

	// def asNumeric(x: Type): Numeric = x match {
	// 	case d: Numeric => x
	// 	case i: Integer => new Numeric(x.storedValue)
	// 	case l: Logical => if (x.storedValue) new Numeric(1.0) else new Numeric(0.0)
	// 	case _ => throw new IllegalArgumentException(s"Unsupported type: ${x.toString}")
	// }

	// def asCharacter(x: Character): Character = x
	// def asCharacter(x: Type): Character = new Character(x.storedValue.toString)
}
