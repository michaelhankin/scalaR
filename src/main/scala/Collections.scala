package scalar

import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag
import TypeUtils._

class RVector(var data: ArrayBuffer[Type], var vtype: String) {
	def apply(idx: Int): Type = {
		return data(idx - 1)
	}

	// return slice 
	def apply(idxMin: Int, idxMax: Int): RVector = {
		return new RVector(data.slice(idxMin, idxMax), vtype)
	}

	def getType: String = vtype
	def length: Int = data.length

	def ==(that: RVector): Boolean = {
		return this.getType == that.getType && this.data == that.data
	}
}

class RList(value: Type*) {

}

// Utilities for the user to work with RVector
object VectorUtils {

}