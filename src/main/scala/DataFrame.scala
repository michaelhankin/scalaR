
package scalar

import scala.collection.mutable._
import VectorUtils._

// DataFrame object in which each RVector in the list corresponds to a column
class DataFrame(var cols: ArrayBuffer[RVector], var schema: Map[String, (Int, String)]) {
	val nCols = cols.length
	// val nRows = cols(0).length

	def apply(row: Int, col: Int): Any = {
		val cell = null
		if (row <= nRows && col <= nCols) {
			val cell = cols(col - 1)(row - 1).storedValue
		} else {
			throw new RuntimeException(s"Error: undefined cells selected")
		}
		cell
	}

	def apply(col: Int): RVector = {
		val colVec = null
		if (col == 0) {
			println(s"data frame with 0 columns and $nRows rows")
		} else if (col <= nCols) {
			val colVec = cols(col - 1)
		} else {
			throw new RuntimeException(s"Error: undefined columns selected")
		}
		colVec
	}

	def apply(col: String): RVector = {
		var idx = 0
		if (schema.contains(col)) {
			idx = schema(col)._1
		} else {
			throw new RuntimeException(s"Error: undefined columns selected")
		}
		cols(idx)
	}

	def getRow(row: Int): ArrayBuffer[Type] = {
		var rowVals = ArrayBuffer[Type]()
		if (row <= nRows) {
			for (col <- cols) {
				rowVals += col(row - 1)
			}
		} else {
			throw new RuntimeException(s"Error: row index is out of bounds")
		}
		rowVals
	}

	def apply(colNames: RVector): ArrayBuffer[RVector] = {
		val colArr = unpackCharacterVector(colNames)
		var result = ArrayBuffer[RVector]()
		for (col <- colArr) {
			result +:= this.apply(col)
		}
		result
	}

	def getRowZeroIndex(row: Int): ArrayBuffer[Type] = {
		var rowVals = ArrayBuffer[Type]()
		if (row <= nRows) {
			for (col <- cols) {
				rowVals += col(row)
			}
		} else {
			throw new RuntimeException(s"Error: row index is out of bounds")
		}
		rowVals
	}

	def printdf() = {
		var widths = new Array[Int](nCols)
		var headerWidths = new Array[Int](nCols)
		var columns = new Array[String](nCols)

		for(header <- schema.keys){
			var index = schema(header)._1
			headerWidths(index) = header.length
			columns(index) = header
		}

		columns = columns
		headerWidths = headerWidths

		for ((col,i) <- cols.zipWithIndex) {
			val w = col.getColWidth
			widths(i) = if (w > headerWidths(i)) w else headerWidths(i)
		}

		// print header
		var sb = new StringBuilder()
		for ((c,j) <- columns.zipWithIndex) {
			for (i <- 0 until widths(j)-c.length){
				sb += ' '
			}
			sb ++= c
			sb ++= "  "
		}
		sb ++= "\n"

		// print data
		for (i <- 1 until nRows+1) {
			val row = this.getRowZeroIndex(i)
			for ((r,k) <- row.zipWithIndex) {
			val curCell = r.toString
				for (j <- 0 until widths(k)-curCell.length) {
					sb += ' '
				}
			sb ++= curCell
			sb ++= "  "
			}
			sb ++= "\n"
		}

		println(sb)
	}

	def head(count: Int = 5) = {
		var widths = new Array[Int](nCols)
		var headerWidths = new Array[Int](nCols)
		var columns = new Array[String](nCols)

		for(header <- schema.keys) {
			var index = schema(header)._1
			headerWidths(index) = header.length
			columns(index) = header
		}

		columns = columns
		headerWidths = headerWidths

		for ((col,i) <- cols.zipWithIndex) {
			val w = col.getColWidth
			widths(i) = if (w > headerWidths(i)) w else headerWidths(i)
		}

		// print header 
		var sb = new StringBuilder()
		for ((c,j) <- columns.zipWithIndex) {
			for (i <- 0 until widths(j)-c.length){
				sb += ' '
			}
			sb ++= c
			sb ++= "  "
		}
		sb ++= "\n"

		// print data 
		for (i <- 1 until count) {
			val row = this.getRowZeroIndex(i)
			for ((r,k) <- row.zipWithIndex) {
			val curCell = r.toString
				for (j <- 0 until widths(k)-curCell.length) {
					sb += ' '
				}
			sb ++= curCell
			sb ++= "  "
			}
			sb ++= "\n"
		}

		println(sb)
	}

	def colnames = {
		var widths = new Array[Int](nCols)
		var headerWidths = new Array[Int](nCols)
		var columns = new Array[String](nCols)

		for(header <- schema.keys){
			var index = schema(header)._1
			headerWidths(index) = header.length
			columns(index) = header
		}

		columns = columns
		headerWidths = headerWidths

		for ((col,i) <- cols.zipWithIndex) {
			val w = col.getColWidth
			widths(i) = if (w > headerWidths(i)) w else headerWidths(i)
		}

		// print header 
		var sb = new StringBuilder()
		for ((c,j) <- columns.zipWithIndex) {
			for (i <- 0 until widths(j)-c.length){
				sb += ' '
			}
			sb ++= c
			sb ++= "  "
		}
		sb ++= "\n"
	}
}

object DataFrameUtils {
	def nrow(df: DataFrame): Int = df.nRows
	def ncol(df: DataFrame): Int = df.nCols
}
