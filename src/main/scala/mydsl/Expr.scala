package mydsl

sealed trait Expr

sealed trait Num                                   extends Expr
final case class IntNum(value: Int)                extends Num
final case class DoubleNum(value: Double)          extends Num
final case class Str(value: String)                extends Expr
final case class Param(name: String)               extends Expr
case object Null                                   extends Expr
final case class Neg(value: Expr)                  extends Expr
final case class Mul(a: Expr, b: Expr)             extends Expr
final case class Div(a: Expr, b: Expr)             extends Expr
final case class Add(a: Expr, b: List[Expr])       extends Expr
sealed trait Bool                                  extends Expr
final case class BoolConst(value: Boolean)         extends Bool
final case class Eq(a: Expr, b: Expr)              extends Bool
final case class Ne(a: Expr, b: Expr)              extends Bool
final case class IfElse(c: Bool, a: Expr, b: Expr) extends Expr
