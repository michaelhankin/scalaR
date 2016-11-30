/**
  * Created by scott on 11/14/16.
  */

package scalar
import scala.reflect._
import scala.collection.mutable._
import VectorUtils._
import DataFrameUtils._
import org.scalatest.FlatSpec
import CsvParser._

class Tests extends FlatSpec {

  "Make types" should "make new Types" in {
    object TypeTest extends ScalaR {
      def run(): Unit = {
        val log = new Logical(true)
        val num_int = new Numeric(0)
        val num_dub = new Numeric(1.0)
        val char = new Character("TEST")
        val na = NA

        assert(log.getType == "Logical")
        assert(num_int.getType == "Numeric")
        assert(num_dub.getType == "Numeric")
        assert(char.getType == "Character")
        assert(na.getType == "Logical")
      }
    }

    TypeTest.run()
  }

  "Simple Vector assignment: Numeric" should "Create Numeric vector from <--" in {
    object SimpAssignNumeric extends ScalaR {
      def run(): Unit = {
        'nvec <-- 1.0
        assert('nvec(1) == 1.0)
        assert(typeOf('nvec) == "Numeric")
      }
    }

    SimpAssignNumeric.run()
  }

   "Simple Vector assignment: Logical" should "Create Logical vector from <--" in {
    object SimpAssignLogical extends ScalaR {
      def run(): Unit = {
        'lvec <-- true
        assert('lvec(1) == true)
        assert(typeOf('lvec) == "Logical")
      }
    }

    SimpAssignLogical.run()
  }

  "Simple Vector assignment: Character" should "Create Character vector from <--" in {
    object SimpAssignCharacter extends ScalaR {
      def run(): Unit = {
        's <-- "Hello"
        assert('s(1) == "Hello")
        assert(typeOf('s) == "Character")
      }
    }

    SimpAssignCharacter.run()
  }

  "c function test: Numeric" should "Create Numeric vector from c()" in {
    object CNumeric extends ScalaR {
      def run(): Unit = {
        'vec <-- c(1,2,3)
        assert(length('vec) == 3)
        assert('vec(1) == 1 && 'vec(2) == 2 && 'vec(3) == 3)
        assert(typeOf('vec) == "Numeric")
      }
    }

    CNumeric.run()
  }

  "c function test: Logical" should "Create Logical vector from c()" in {
    object CLogical extends ScalaR {
      def run(): Unit = {
        'vec <-- c(true,true,false)
        assert(length('vec) == 3)
        assert('vec(1) == true && 'vec(2) == true && 'vec(3) == false)
        assert(typeOf('vec) == "Logical")
      }
    }

    CLogical.run()
  }

  "c function test: Character" should "Create Character vector from c()" in {
    object CCharacter extends ScalaR {
      def run(): Unit = {
        'vec <-- c("hello", "world", "vector")
        assert(length('vec) == 3)
        assert('vec(1) == "hello" && 'vec(2) == "world" && 'vec(3) == "vector")
        assert(typeOf('vec) == "Character")
      }
    }

    CCharacter.run()
  }

  "c function test: mixed type 1" should "Create Numeric with Logical from c()" in {
    object CMix1 extends ScalaR {
      def run(): Unit = {
        'vec <-- c(true, 1, 1.23)
        assert(length('vec) == 3)
        assert('vec(1) == 1.0 && 'vec(2) == 1.0 && 'vec(3) == 1.23)
        assert(typeOf('vec) == "Numeric")
      }
    }

    CMix1.run()
  }

  "c function test: mixed type 2" should "Create Character with Numeric and Logical from c()" in {
    object CMix2 extends ScalaR {
      def run(): Unit = {
        'vec <-- c(true, 1.0, "string")
        assert(length('vec) == 3)
        assert('vec(1) == "true" && 'vec(2) == "1.0" && 'vec(3) == "string")
        assert(typeOf('vec) == "Character")
      }
    }

    CMix2.run()
  }

  // just add RVector constructor that takes an RVector
  // and 'unroll' it. eg c(c(1,2,3),c(4,5,6)) == RVector(1,2,3,4,5,6)
  "c function test: nested" should "unroll" in {
    object CMix2 extends ScalaR {
      def run(): Unit = {
        'vec <-- c(c(1,2,3), c(4,5,6), c(7,8,9))
      }
    }

    CMix2.run()
  }

  // slicing equality is weird, probably not that important however
  "slicing test" should "return proper slice" in {
    object SliceTest extends ScalaR {
      def run(): Unit = {
        'vec <-- c(1,2,3,4,5)
        val x = 'vec(1 to 3).data

        val stream = new java.io.ByteArrayOutputStream()
        Console.withErr(stream) {
      //  println(s"${x}")
      //  println(s"${c(1.0, 2.0, 3.0).data}")
        }

        'vec <-- c(1,2,3,4,5)
        assert(length('vec) == 5)
        assert(length('vec(1 to 3)) == 3)
        assert('vec(1 to 3) == c(1.0, 2.0, 3.0))
      }
    }

    SliceTest.run()
  }


  "DataFrame creation test" should "create a DataFrame" in {
    object DFCreationTest extends ScalaR {
      def run(): Unit = {
        val cols = ArrayBuffer[RVector](c(1, 2, 3), c(2, 4, 6), c("a", "b", "c"))
        val schema = Map[String, (Int, String)]("fuck" -> (1, "Numeric"), "this" -> (2, "Numeric"), "shit" -> (3, "Character"))

        val df = new DataFrame(cols, schema)
        assert(df(2, 2) == 4)
        assert(df(1, 1) == 1)
        assert(df("fuck") == c(1, 2, 3))
      }
    }
  }

  // "DataFrameUtilities test" should "return proper values from utility functions" in {
  //   object DFUtilitiesTest extends ScalaR {
  //     def run(): Unit = {

  //     }
  //   }
  // }

  "mean test" should "report mean" in {
    object MeanTest extends ScalaR {
      def run(): Unit = {
        'nvec <-- c(1,2,3,4,5)
        assert(mean('nvec)(1).storedValue == 3.0)
        'lvec <-- c(true, false, true, true)
        assert(mean('lvec)(1).storedValue == 0.75)
      }
    }

    MeanTest.run()
  }

  "standard deviation test" should "report standard deviation" in {
    object StdDevTest extends ScalaR {
      def run(): Unit = {
        'nvec <-- c(1,3,5)
        assert(sd('nvec)(1).storedValue == 2.0)
        'lvec <-- c(1,0,1,1)
        assert(sd('lvec)(1).storedValue == 0.5)
      }
    }

    StdDevTest.run()
  }


  "csv parser test" should "correctly make a dataframe" in {
    object CsvParse {
      def run(): Unit = {
        val stream = new java.io.ByteArrayOutputStream()
        Console.withErr(stream) {
        var buff = (ArrayBuffer[RVector](), Map[String, (Int,String)]())
        setPath("./")
        buff = CsvParser.read_csv("test_i.csv", true, ",", "na")
        var data = new DataFrame(buff._1, buff._2)
        data.printdf()

        }
      }
    }

    CsvParse2.run()
  }

  "RVector toString" should "get string repr" in {
    object ToStringRVec extends ScalaR {
      def run(): Unit = {
        assert(c(1,2,3,4,5).toString == "[1] 1.0 2.0 3.0 4.0 5.0")
      }
    }

    ToStringRVec.run()
  }

  "RVector colwidth" should "get longest column" in {
    object ColWidthTest extends ScalaR {
      def run(): Unit = {
        assert(c("Toyota", "Honda", "Hyundai").getColWidth == 7)
      }
    }

    ColWidthTest.run()
  }









}
