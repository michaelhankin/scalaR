/**
  * Created by scott on 11/14/16.
  */

package scalar
import scala.reflect._
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

  "c function test: Integer" should "Create Integer vector from c()" in {
    object CfuncInteger extends ScalaR {
      def run(): Unit = {
        'vec <-- c(1,2,3)
        assert(length('vec) == 3)
        assert('vec(1) == 1 && 'vec(2) == 2 && 'vec(3) == 3)
        assert(typeOf('vec) == "Numeric")
      }
    }

    CfuncInteger.run()
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
