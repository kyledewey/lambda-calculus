sealed trait Exp

case class Variable(name: String) extends Exp
case class Lambda(x: Variable, body: Exp) extends Exp
case class Apply(e1: Exp, e2: Exp) extends Exp
