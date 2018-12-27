package org.manatki.glass

import cats.arrow._
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.option._


trait OpticCompanion[O[s, t, a, b] >: PSame[s, t, a, b]] {
  self =>
  type Context <: OpticContext
  type Mono[a, b] = O[a, a, b, b]

  def apply[S, T, A, B](implicit o: O[S, T, A, B]): O[S, T, A, B] = o

  def compose[S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B]): O[S, T, U, V]

  def toGeneric[S, T, A, B](o: O[S, T, A, B]): Optic[Context, S, T, A, B]
  def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): O[S, T, A, B]

  implicit val category: Category[Mono] = new Category[Mono] {
    def id[A]                                                      = PSame.id[A, A]
    def compose[A, B, C](f: Mono[B, C], g: Mono[A, B]): Mono[A, C] = self.compose(f, g)
  }
}

trait OpticContext {
  type F[_]
  type P[_, _]
}

abstract class MonoOpticCompanion[PO[s, t, a, b] >: PSame[s, t, a, b]](poly: OpticCompanion[PO]) {
  self =>
  type O[a, b] = PO[a, a, b, b]

  def apply[A, B](implicit o: O[A, B]): O[A, B] = o

  def compose[A, B, C](f: O[B, C], g: O[A, B]): O[A, C] = poly.compose(f, g)
}

/** precisely implicit A => B deriveable from both Extract and Downcast */
trait Transform[A, B] {
  def apply(a: A): B
}

object Transform extends TransformInstances {
  final implicit def fromExtract[A, B](implicit extract: Extract[A, B]): Transform[A, B] = extract.extract
}
trait TransformInstances {
  final implicit def fromUpcast[A, B](implicit upcast: Upcast[A, B]): Transform[B, A] = upcast.upcast
}
/** generic optic form, aka Profunctor Optic */
trait Optic[-Ctx <: OpticContext, S, T, A, B] {
  self =>
  def apply(c: Ctx)(p: c.P[A, c.F[B]]): c.P[S, c.F[T]]

  def andThen[C1 <: Ctx, U, V](that: Optic[C1, A, B, U, V]): Optic[C1, S, T, U, V] =
    new Optic[C1, S, T, U, V] {
      def apply(c: C1)(p: c.P[U, c.F[V]]): c.P[S, c.F[T]] = self(c)(that(c)(p))
    }
}

object Optic {
  type Mono[C <: OpticContext, A, B] = Optic[C, A, A, B, B]
  def id[Ctx <: OpticContext, S, A]: Optic[Ctx, S, A, S, A] = PSame.toGeneric(PSame.id)

  implicit def opticCategoryInstance[Ctx <: OpticContext]: Category[Mono[Ctx, ?, ?]] =
    new Category[Mono[Ctx, ?, ?]] {
      def id[A]: Mono[Ctx, A, A]                                                    = Optic.id
      def compose[A, B, C](f: Mono[Ctx, B, C], g: Mono[Ctx, A, B]): Mono[Ctx, A, C] = g.andThen(f)
    }
}

/** reversed version of cats.data.Const */
final case class Tagged[A, B](value: B) extends AnyVal {
  def map[C](f: B => C): Tagged[A, C] = Tagged(f(value))
  def retag[C]: Tagged[C, B]          = Tagged(value)
}

object Tagged {
  implicit val profunctor: Compose[Tagged] with PChoice[Tagged] = new Compose[Tagged] with PChoice[Tagged] {
    def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(g: B => D): Tagged[C, D]    = fab.map(g).retag
    def compose[A, B, C](f: Tagged[B, C], g: Tagged[A, B]): Tagged[A, C]            = f.retag
    def left[A, B, C](pab: Tagged[A, B]): Tagged[Either[A, C], Either[B, C]]        = pab.map(_.asLeft[C]).retag
    def right[A, B, C](pab: Tagged[A, B]): Tagged[Either[C, A], Either[C, B]]       = pab.map(_.asRight[C]).retag
    override def optional[A, B, C](pab: Tagged[A, B]): Tagged[Option[A], Option[B]] = pab.map(_.some).retag
  }
}

/** non-category version of cats.arrow.choice */
trait PChoice[P[_, _]] extends Profunctor[P] {
  def left[A, B, C](pab: P[A, B]): P[Either[A, C], Either[B, C]]
  def right[A, B, C](pab: P[A, B]): P[Either[C, A], Either[C, B]]
  def optional[A, B, C](pab: P[A, B]): P[Option[A], Option[B]] =
    dimap(right[A, B, Unit](pab))(Either.fromOption(_: Option[A], ()))(_.toOption)
}

object PChoice {
  def apply[P[_, _]](implicit P: PChoice[P]): PChoice[P] = P

  implicit val functionInstance: PChoice[? => ?] = new PChoice[? => ?] {
    def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C]        = _.leftMap(pab)
    def right[A, B, C](pab: A => B): Either[C, A] => Either[C, B]       = _.map(pab)
    def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D    = f andThen fab andThen g
    override def optional[A, B, C](pab: A => B): Option[A] => Option[B] = _.map(pab)
  }
}
