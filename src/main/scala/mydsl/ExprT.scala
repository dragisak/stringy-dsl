package mydsl

import cats.syntax.all._
import cats._
import higherkindness.droste.util.DefaultTraverse

sealed trait ExprT[A]

final case class NumT[A](value: Int)           extends ExprT[A]
final case class StrT[A](value: String)        extends ExprT[A]
final case class ParamT[A](name: String)       extends ExprT[A]
final case class NullT[A]()                    extends ExprT[A]
final case class NegT[A](value: A)             extends ExprT[A]
final case class AddT[A](a: A, b: List[A])     extends ExprT[A]
sealed trait BoolT[A]                          extends ExprT[A]
final case class BoolConstT[A](value: Boolean) extends BoolT[A]
final case class EqT[A](a: A, b: A)            extends BoolT[A]
final case class NeT[A](a: A, b: A)            extends BoolT[A]
final case class IfElseT[A](c: A, a: A, b: A)  extends ExprT[A]

object ExprT {

  implicit val exprTraverse: Traverse[ExprT] =
    new DefaultTraverse[ExprT] {
      override def traverse[G[_]: Applicative, A, B](fa: ExprT[A])(f: A => G[B]): G[ExprT[B]] = fa match {
        case v: NumT[B @unchecked]       => (v: ExprT[B]).pure[G]
        case v: StrT[B @unchecked]       => (v: ExprT[B]).pure[G]
        case v: ParamT[B @unchecked]     => (v: ExprT[B]).pure[G]
        case v: NullT[B @unchecked]      => (v: ExprT[B]).pure[G]
        case v: BoolConstT[B @unchecked] => (v: ExprT[B]).pure[G]
        case NegT(x)                     => f(x).map(NegT(_))
        case AddT(x, y)                  => (f(x), y.traverse(f)).mapN(AddT(_, _))
        case EqT(x, y)                   => (f(x), f(y)).mapN(EqT(_, _))
        case NeT(x, y)                   => (f(x), f(y)).mapN(NeT(_, _))
        case IfElseT(c, x, y)            => (f(c), f(x), f(y)).mapN(IfElseT(_, _, _))
      }
    }
}
