package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    def isEven: (Int => Boolean) = (n: Int) => (n % 2 == 0)
    def isOdd: (Int => Boolean) = (n: Int) => (if (n > 0) (n % 2 == 1) else (n % 2 == -1))
    // val emptySet: Set = (n: Int) => false
    def gt(m: Int)(n: Int): Boolean = n > m
    def lt(m: Int)(n: Int): Boolean = n < m
    def setEquals(m: Int)(n: Int): Boolean = m == n
    // def addToSet(s: Set, x: Int): Set = union(s, singletonSet(x))
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("singletonSet(2) doesn't contain 1") {
    new TestSets {
      assert(!contains(s2, 1), "s2 doesn't contain 1")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      val u = union(s1, union(s2, union(s3, singletonSet(5))))
      val v = union(s1, union(s2, union(singletonSet(-4), singletonSet(300))))
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      assert(contains(union(u, v), 1))
      assert(contains(union(u, v), 2))
      assert(contains(union(u, v), 3))
      assert(contains(union(u, v), 5))
      assert(contains(union(u, v), -4))
      assert(contains(union(u, v), 300))
    }
  }

  test("intersection contains elements in both sets"){
    new TestSets {
      val s = union(s1, s2)
      val sInt = intersect(s1, s)
      assert(contains(sInt, 1), "intersection of s1 and s2 contains 1")
      assert(!contains(sInt, 3), "intersection of s1 and s2 does not contain 3")
    }
  }

  test("diff(A, B) contains things in A that are not in B") {
    new TestSets {
      val s12 = union(s1, s2) 
      // s1 U s2 has 1 and 2 in it. diff(s1 U s2, s3) has 1 and 2 in it, but not 3
      val diff_1U2_3 = diff(s12, s3)
      assert(contains(diff_1U2_3, 1), "s1 U s2 has 1 and 2 in it. diff(s1 U s2, s3) has 1")
      assert(contains(diff_1U2_3, 2), "s1 U s2 has 1 and 2 in it. diff(s1 U s2, s3) has 2")
      assert(!contains(diff_1U2_3, 3), "s1 U s2 has 1 and 2 in it. diff(s1 U s2, s3) doesn't have 3")
    }
  }

  test("filter out elements not in s") {
    new TestSets {
      val s123 = union(union(s1, s2), s3)
      // val p: Int => Boolean = (x: Int) => x > 2
      def p(x: Int): Boolean = x > 2
      assert(contains(filter(s123, p), 3), "3 is larger than 2")
      assert(!contains(filter(s123, p), 1), "1 is smaller than 2")
      assert(!contains(filter(s123, p), 2), "2 is equal to 2")

      val evens = filter(union(union(s2, singletonSet(4)), singletonSet(6)), isEven)
      val odds = filter(union(union(s1, s3), singletonSet(5)), isOdd)
      assert(contains(evens, 6), "even ints contains 6")
      assert(!contains(evens, 5), "even ints contains 5")
    }
  }

  test("Set was built correctly") {
    new TestSets {
      val intSet = (x: Int) => (x >= 1 && x <= 5)
      (1 to 5) foreach (n => assert(contains(intSet, n), n + " is in the set"))
      assert(!contains(intSet, 6), "6 is not in Set(1,2,3,4,5)")
      assert(contains(intSet, 3), "3 is in Set(1,2,3,4,5)")
    }
  }

  test("All elements in s fulfill p") {
    new TestSets {
      val ints = (x: Int) => (x >= -bound && x <= bound)
      val evens = filter(ints, isEven)
      val odds = filter(ints, isOdd)
      val gt5 = filter(ints, gt(5))
      val lt5 = filter(ints, lt(5))
      val eq5 = filter(ints, setEquals(5))

      (-bound to bound) foreach (n => assert(contains(ints, n), n + " is in the set"))
      assert(forall(evens, isEven), "The set of even integers contains only even integers")
      assert(!forall(evens, isOdd), "The set of even integers doesn't contain odd integers")
      assert(forall(odds, isOdd), "The set of odd integers contains only odd integers")
      assert(!forall(odds, isEven), "The set of odd integers doesn't contain even integers")
      assert(forall(emptySet, isEven), "the empty set passes any test")
      assert(forall(gt5, gt(5)(_)), "each element is > 5")
      assert(forall(lt5, lt(5)(_)), "each element is < 5")
      assert(forall(eq5, setEquals(5)(_)), "the element is = 5")
      assert(!forall(eq5, setEquals(6)(_)), "the element is != 6")
      assert( !forall(lt5, lt(-5)(_)), "not all of the elements are < -5")
    }
  }

  test("Does any element in s fulfill p?") {
    new TestSets {
      val ints = (x: Int) => (x >= -bound && x <= bound)
      val evens = filter(ints, isEven)
      val odds = filter(ints, isOdd)
      val gt5 = filter(ints, gt(5))

      (-bound to bound) foreach (n => assert(contains(ints, n), n + " is in the set"))
      assert(exists(evens, isEven), "The set of even integers contains at least one even integer")
      assert(!exists(evens, isOdd), "The set of even integers doesn't contain at least one odd integer")
      assert(exists(odds, isOdd), "The set of odd integers contains at least one odd integer")
      // assert(exists(emptySet, isEven), "empty set passes all tests")
    }
  }

  test("Map function works correctly") {
    new TestSets {
      val s123 = (x: Int) => (x >= 1 && x <= 3)
      val s246 = (x: Int) => (x == 2 || x == 4 || x == 6)
      val newSet = map(s123, x => 2 * x)

      assert(contains(newSet, 2), "2 should be in the resulting set")
      assert(contains(newSet, 4), "4 should be in the resulting set")
      assert(contains(newSet, 6), "6 should be in the resulting set")
      assert(!contains(newSet, 3), "3 should not be in the resulting set")
      assert(!contains(newSet, -2), "-2 should not be in the resulting set")
    }
  }

  test("Forall map doubling") {
    new TestSets {
      val ints = (x: Int) => (x >= 1 && x <= bound / 2)
      val doubled = map(ints, x => 2 * x)
      assert(forall(doubled, isEven), "each entry should be even")
    }
  }
}
