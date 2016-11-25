package scalar

import scala.collection.mutable.ArrayBuffer
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag
import TypeUtils._

class RVector(var data: ArrayBuffer[Type], var vtype: String) {
	def apply(idx: Int): Type = {
		return data(idx)
	}

	def getType: String = vtype
	def length: Int = data.length
}

class RList(value: Type*) {

}

// Utilities for the user to work with RVector
object VectorUtils {
	def length(vec: RVector): Int = vec.data.length

	def asLogical(vec: RVector): RVector = {
		var buf = ArrayBuffer[Type]() ++ vec.data.map(toLogical)
		return new RVector(buf, "Logical")
	}

	def asNumeric(vec: RVector): RVector = {
		var buf = ArrayBuffer[Type]() ++ vec.data.map(toNumeric)
		return new RVector(buf, "Numeric")
	}
	def asCharacter(vec: RVector): RVector = {
		var buf = ArrayBuffer[Type]() ++ vec.data.map(toCharacter)
		return new RVector(buf, "Character")
	}
}