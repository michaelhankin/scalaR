package ut.cs.cs345.scalar

import scala.collection.mutable.ArrayBuffer

class Vector(value: ArrayBuffer[Type]) extends Type {
	def storedValue: ArrayBuffer[Type] = value
	def getType(): String = "Vector"
}

class RList(value: Type*) {

}
