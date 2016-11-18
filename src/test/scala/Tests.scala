/**
  * Created by scott on 11/14/16.
  */

package ut.cs.cs345.scalar
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

  "assignment_test" should "allow assignment from symbol" in {
    object Test2 extends ScalaR {
      def run(): Unit = {
        's <-- 1
        assert('s(1) == 1)
      }
    }

    Test2.run()
  }

  // "can reference var" should "allow reference to " in {
  //   object Test2 extends ScalaR {
  //     def run(): Unit = {
  //       // val s = Symbol("s")
  //       's <-- true
  //       assert('s(1) == true)
  //     }
  //   }

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
