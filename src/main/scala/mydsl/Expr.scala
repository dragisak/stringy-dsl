package mydsl

sealed trait Expr

final case class Num(value: Int)                   extends Expr
final case class Str(value: String)                extends Expr
final case class Param(name: String)               extends Expr
case object Null                                   extends Expr
final case class Add(a: Expr, b: List[Expr])       extends Expr
sealed trait Bool                                  extends Expr
final case class Eq(a: Expr, b: Expr)              extends Bool
final case class Ne(a: Expr, b: Expr)              extends Bool
final case class IfElse(c: Bool, a: Expr, b: Expr) extends Expr

sealed trait ExprT[+A]

final case class NumT[A](value: Int)                   extends ExprT[A]
final case class StrT[A](value: String)                extends ExprT[A]
final case class ParamT[A](name: String)               extends ExprT[A]
final case class NullT[A]()                            extends ExprT[A]
final case class AddT[A](a: Expr, b: List[Expr])       extends ExprT[A]
sealed trait BoolT[A]                                  extends ExprT[A]
final case class EqT[A](a: Expr, b: Expr)              extends BoolT[A]
final case class NeT[A](a: Expr, b: Expr)              extends BoolT[A]
final case class IfElseT[A](c: Bool, a: Expr, b: Expr) extends ExprT[A]
