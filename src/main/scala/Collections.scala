package ut.cs.cs345.scalar

import scala.collection.mutable.ArrayBuffer
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

class RVector[+T](val data: ArrayBuffer[T @uncheckedVariance])(implicit ev: ClassTag[T]) {
	val vecType = data(0)

	def apply(idx: Int): T = {
		return data(idx)
	}

	// def data: ArrayBuffer[Type] = value
	def getType(): String = ev.toString()

	def length: Int = data.length
}

// object c {
// 	val typeHierarchy = Array("Logical", "Integer", "Numeric", "Character")

// 	def apply(values: Any*): Vector = {
// 		var highestType: String = "Logical" 
// 		for (value <- values) {
// 			val currType = value.getClass match {
// 				case currType: java.lang.Integer => "Integer"
// 				case java.lang.String => "Character"
// 				case java.lang.Double => "Numeric"
// 				case java.lang.Boolean => "Logical"
// 				case _ => "Unknown"
// 			}
// 			val currIdx = typeHierarchy.indexOf(currType)
// 			if (currIdx != -1) {
// 				if (typeHierarchy.indexOf(currType) > typeHierarchy.indexOf(highestType)) {
// 					highestType = currType
// 				}
// 			} else {
// 				throw new RuntimeException(s"Error: element at index $currIdx has invalid type")
// 			}
// 		}
// 		var vecArr: ArrayBuffer[Type] = highestType match {
// 			case "Integer" => values.map(asInteger).to[ArrayBuffer]
// 			case "Character" => values.map(asCharacter).to[ArrayBuffer]
// 			case "Numeric" => values.map(asNumeric).to[ArrayBuffer]
// 			case "Logical" => values.map(asLogical).to[ArrayBuffer]
// 		}
// 		Vector(vecArr)
// 	}
// }

class RList(value: Type*) {

}
