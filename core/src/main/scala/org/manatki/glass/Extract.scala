package org.manatki.glass

import cats.data.Const
import cats.{Functor, Monoid, Semigroup}

/** aka Getter
  * A has exactly one B
  * mere function from A to B
  * and part of Lens
  */
trait PExtract[S, T, A, B] extends PDowncast[S, T, A, B] with PReduced[S, T, A, B] {
  def extract(s: S): A

  def downcast(s: S): Option[A]                           = Some(extract(s))
  def reduceMap[X: Semigroup](s: S)(f: A => X): X     = f(extract(s))
  override def foldMap[X: Monoid](s: S)(f: A => X): X = f(extract(s))
}

object Extract extends MonoOpticCompanion(PExtract)

object PExtract extends OpticCompanion[PExtract] {
  def compose[S, T, A, B, U, V](f: PExtract[A, B, U, V], g: PExtract[S, T, A, B]): PExtract[S, T, U, V] =
    s => f.extract(g.extract(s))

  trait Context extends PContains.Context {
    type X
    type F[A] = Const[X, A]
    def functor: Functor[Const[X, ?]] = Functor[Const[X, ?]]
  }
  override def toGeneric[S, T, A, B](o: PExtract[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => Const[c.X, B]): S => Const[c.X, T] = s => p(o.extract(s)).retag
    }
  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PExtract[S, T, A, B] =
    s => o(new Context { type X = A })(a => Const(a))(s).getConst
}
