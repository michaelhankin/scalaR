
package scalar

import scala.collection.mutable._
import VectorUtils._

// DataFrame object in which each RVector in the list corresponds to a column
class DataFrame(var cols: ArrayBuffer[RVector], var schema: LinkedHashMap[String, (Int, String)]) {
	val nCols = cols.length
	val nRows = if (cols.length > 0) cols(0).length else 0

	def printData = {
		for (col <- cols) {
			col.getType match {
				case "Logical" => println(unpackLogicalVector(col))
				case "Numeric" => println(unpackNumericVector(col))
				case "Character" => println(unpackCharacterVector(col))
				case _ => println("")
			}
		}
	}

	def apply(row: Int, col: Int): RVector = {
		if (row <= nRows && col <= nCols) {
			val cell = cols(col - 1)(row).data(row-1)
			val cellType = CsvParser.infer_type(cell.toString)
			return new RVector(ArrayBuffer[Type](cell), cellType)
		} else {
			throw new RuntimeException(s"Error: undefined cells selected")
		}
		return new RVector(ArrayBuffer[Type](), "Logical")
	}

	def apply(col: Int): RVector = {
		if (col == 0) {
			println(s"data frame with 0 columns and $nRows rows")
		} else if (col <= nCols) {
			return this.cols(col - 1)
		} else {
			throw new RuntimeException(s"Error: undefined columns selected")
		}
		return new RVector(ArrayBuffer[Type](), "Logical")
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
				rowVals += col(row).data(0)
			}
		} else {
			throw new RuntimeException("Error: row index is out of bounds")
		}
		rowVals
	}

	def apply(colNames: RVector): DataFrame = {
		val colArr = unpackCharacterVector(colNames)
		var newSchema = LinkedHashMap[String, (Int, String)]()
		for ((colName, i) <- colArr.zipWithIndex) {
			val colType = schema(colName)._2
			newSchema += (colName -> (i + 1, colType))
		}
		var newData = ArrayBuffer[RVector]()
		for (col <- colArr) {
			newData +:= this.apply(col)
		}
		new DataFrame(newData, newSchema)
	}

	def getRowZeroIndex(row: Int): ArrayBuffer[Type] = {
		var rowVals = ArrayBuffer[Type]()
		if (row <= nRows) {
			for (col <- cols) {
				rowVals += col(row-1).data(0)
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
			val row = this.getRow(i)
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
			val row = this.getRow(i)
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
