package scalar

import scala.collection.mutable.ArrayBuffer

// DataFrame object in which each RVector in the list corresponds to a column
class DataFrame(var cols: ArrayBuffer[RVector], var schema: Map[String, (Int, String)]) {
	val nCols = cols.length
	val nRows = cols(0).length

	def apply(row: Int, col: Int): Any = {
		if (row < nRows && col < nCols) {
			val cell = cols(col)(row)
		} else {
			if (row >= nRows && col >= nCols) {
				// Return NULL
			} else if (row >= nRows) {
				// Return NA
			} else {
				throw new RuntimeException(s"Error: undefined columns selected")
			}
		}
		cell
	}

	def apply(col: Int): RVector = {
		if (col == 0) {
			println(s"data frame with 0 columns and $nRows rows")
			// Return NULL (?)
		} else if (col < nCols) {
			val colVec = cols(col)
		} else { 
			throw new RuntimeException(s"Error: undefined columns selected")
		}
		colVec
	}

	def apply(col: String): RVector = {
		if (schema.contains(col)) {
			val idx = schema(col)._1
		} else {
			throw new RuntimeException(s"Error: undefined columns selected")
		}
		cols(idx)
	}

	def apply(colNames: RVector): ArrayBuffer[RVector] = {
		
	}
}

object DataFrameUtils {
	def nrows(df: DataFrame): Int = df.nRows
	def ncols(df: DataFrame): Int = df.nCols
}