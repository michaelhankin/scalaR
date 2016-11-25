/**
  * Created by scott on 11/14/16.
  */

package scalar
import scala.reflect._
import VectorUtils._
import org.scalatest.FlatSpec

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
        println(s"${x}")
        println(s"${c(1.0, 2.0, 3.0).data}")
        }

        'vec <-- c(1,2,3,4,5)
        assert(length('vec) == 5)
        assert(length('vec(1 to 3)) == 3)
        assert('vec(1 to 3) == c(1.0, 2.0, 3.0))
      }
    }

    SliceTest.run()
  }

  //   Test2.run()
  // }

  // "make vector" should "allow reference to " in {
  //   object VectorTest extends ScalaR {
  //     def run(): Unit = {
  //       's <-- 1
  //       's(1)
  //     }
  //   }

  //   Test2.run()
  // }
}
