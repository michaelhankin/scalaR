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
	def storedValue: ArrayBuffer[Type] = data

	def ==(that: RVector): Boolean = {
		return this.getType == that.getType && this.data == that.data
	}

	def getColWidth: Int = {
		var max = 0
		for (v <- this.data) {
			if (v.storedValue.toString.length > max)
				max = v.storedValue.toString.length
		}
		return max 
	}

	override def toString: String = {
		var str = "[1]"
		for (v <- this.data) {
			str += s" ${v.toString}"
		}
		return str
	}
}

class RList(value: Type*) {

}

// Utilities for the user to work with RVector
object VectorUtils {
	def unpackNumericVector(vec: RVector): List[Double] = {
		if (vec.getType != "Numeric")
			throw new IllegalArgumentException("Vector is not Numeric")

		var retval = List[Double]()
		for (v <- vec.data) {
			v.storedValue match {
				case d: Double => retval +:= d
			}
		}
		return retval
	} 

	def unpackLogicalVector(vec: RVector): List[Boolean] = {
		if (vec.getType != "Logical")
			throw new IllegalArgumentException("Vector is not Logical")
		
		var retval = List[Boolean]()
		for (v <- vec.data) {
			v.storedValue match {
				case b: Boolean => retval +:= b
			}
		}
		return retval
	}

	def unpackCharacterVector(vec: RVector): List[String] = {
		if (vec.getType != "Character")
			throw new IllegalArgumentException("Vector is not Character")

		var retval = List[String]()
		for (v <- vec.data) {
			v.storedValue match {
				case s: String => retval +:= s
			}
		}
		return retval
	}
}