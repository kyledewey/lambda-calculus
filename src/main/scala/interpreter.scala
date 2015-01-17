object InterpreterDefs {
  type Env = Map[Variable, Closure]
}
import InterpreterDefs._

case class Closure(function: Lambda, env: Env)

object Interpreter {
  def interpret(e: Exp, env: Env): Closure = {
    e match {
      case x: Variable => {
        // return the value of the variable in the environment
        env(x)
      }
      case f: Lambda => {
        // create a closure from the function along with the current
        // environment
        Closure(f, env)
      }
      case Apply(e1, e2) => {
        // evaluate the parameter passed to the function down to a value
        val actualParam = interpret(e2, env)

        // evaluate the expression on the left, which should result
        // in a closure.
        val Closure(Lambda(x, body), fEnv) = interpret(e1, env)

        // evaluate the function in the closure underneath the environment
        // captured in the closure, passing along the parameter in the
        // closure's environment.  Note that we don't pass along the outer
        // environment - by the very nature of closures, everything the
        // closure needs is contained in the closure's environment.
        interpret(body, fEnv + (x -> actualParam))
      }
    }
  }

  def interpret(e: Exp): Closure = interpret(e, Map())
}
