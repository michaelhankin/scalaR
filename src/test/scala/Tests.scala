/**
  * Created by scott on 11/14/16.
  */

package scalar
import scala.reflect._
import org.scalatest.FlatSpec

class Tests extends FlatSpec {

  "basic_test" should "make new Logical" in {
    object Test1 extends ScalaR {
      def run(): Unit = {
        var bool = new Logical(true) 
        assert(bool.getType() == "Logical") 
      }
    }

    Test1.run()
  }

  "Simple Vector assignment: Integer" should "Create Integer vector from <--" in {
    object SimpAssignInteger extends ScalaR {
      def run(): Unit = {
        's <-- 1
        assert('s(1) == 1)
        assert('s.getType() == classTag[Integer].toString())
      }
    }

    SimpAssignInteger.run()
  }

  "Simple Vector assignment: Numeric" should "Create Numeric vector from <--" in {
    object SimpAssignDouble extends ScalaR {
      def run(): Unit = {
        's <-- 1.0
        assert('s(1) == 1.0)
        assert('s.getType() == classTag[Numeric].toString())
      }
    }

    SimpAssignDouble.run()
  }

   "Simple Vector assignment: Logical" should "Create Logical vector from <--" in {
    object SimpAssignLogical extends ScalaR {
      def run(): Unit = {
        's <-- true
        assert('s(1) == true)
        assert('s.getType() == val tag = classTag[Logical].toString())
      }
    }

    SimpAssignLogical.run()
  }

  "Simple Vector assignment: Character" should "Create Character vector from <--" in {
    object SimpAssignCharacter extends ScalaR {
      def run(): Unit = {
        's <-- "Hello"
        assert('s(1) == "Hello")
        assert('s.getType() == val tag = classTag[Character].toString())
      }
    }

    SimpAssignCharacter.run()
  }

  "c function test: Integer" should "Create Integer vector from c()" in {
    object CfuncInteger extends ScalaR {
      def run(): Unit = {
        'vec <-- c(1,2,3)
        assert('vec.length == 3)
        assert('vec(1) == 1 && 'vec(2) == 2 && 'vec(3) == 3)
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
