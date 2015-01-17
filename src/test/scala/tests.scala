import org.scalatest._
import org.scalatest.Assertions._

class Tests extends FlatSpec with Matchers {
  import Interpreter.interpret

        
  // the identity function
  val id = Lambda(Variable("x"), Variable("x")) 

  "The interpreter" should "evaluate id" in {
    assertResult(id) {
      interpret(id).function
    }
  }

  it should "evaluate self-apply id" in {
    assertResult(id) {
      interpret(Apply(id, id)).function
    }
  }

  val zero = Lambda(Variable("f"),
                    Lambda(Variable("x"),
                           Variable("x")))

  // the successor function
  val succ =
    Lambda(Variable("n"),
           Lambda(Variable("f"),
                  Lambda(Variable("x"),
                         Apply(Variable("f"),
                               Apply(Variable("n"),
                                     Apply(Variable("f"),
                                           Variable("x")))))))

  it should "evaluate successor of zero to be one" in {
    assertResult(id) {
      // We need to get the number down into some sort of normal form
      // which can be compared, hence all the extra applies.  There
      // are actually an infinite number of ways to encode the
      // same number in lambda calculus, which is because in the
      // raw, something like 5 and 2 + 3 end up having different encodings
      // until we fully evaluate them.  This odd property is ultimately
      // because the language is not strongly normalizing, which
      // comes from the fact that it is Turing-complete.

      interpret(
        Apply(Apply(Apply(succ, zero),
              id), id)).function
    }
  }
}
