package org.manatki.glass

/** polymorphic equality: any relation for S and T equal to relation of A and B */
trait PSame[S, T, A, B] extends PEquivalent[S, T, A, B] {
  self =>
  def rsubst[R[_, _]](r: R[S, T]): R[A, B]

  def upcast(b: B): T                     = inverse.rsubst[λ[(x, y) => x]](b)
  def extract(a: S): A                    = rsubst[λ[(x, y) => x]](a)
  override def inverse: PSame[B, A, T, S] = rsubst[λ[(x, y) => PSame[y, x, T, S]]](PSame.id)
  def swap: PSame[T, S, B, A]             = rsubst[λ[(x, y) => PSame[T, S, y, x]]](PSame.id)
  def flip: PSame[A, B, S, T]             = rsubst[λ[(x, y) => PSame[x, y, S, T]]](PSame.id)
}

object Same extends MonoOpticCompanion(PSame){
  def id[A]: Same[A, A] = PSame.id[A, A]
}

object PSame extends OpticCompanion[PSame] {
  type Context             = OpticContext
  override type Mono[A, B] = Same[A, B]

  private def refl[A, B]: PSame[A, B, A, B] = new PSame[A, B, A, B] {
    def rsubst[K[_, _]](k: K[A, B]): K[A, B] = k
  }

  private val anyId               = refl[Any, Any]
  def id[A, B]: PSame[A, B, A, B] = anyId.asInstanceOf[PSame[A, B, A, B]]

  def compose[S, T, A, B, U, V](f: PSame[A, B, U, V], g: PSame[S, T, A, B]): PSame[S, T, U, V] =
    f.rsubst[PSame[S, T, ?, ?]](g)

  override def toGeneric[S, T, A, B](o: PSame[S, T, A, B]): Optic[OpticContext, S, T, A, B] =
    new Optic[OpticContext, S, T, A, B] {
      def apply(c: OpticContext)(p: c.P[A, c.F[B]]): c.P[S, c.F[T]] = o.flip.rsubst[λ[(x, y) => c.P[x, c.F[y]]]](p)
    }

  override def fromGeneric[S, T, A, B](o: Optic[OpticContext, S, T, A, B]): PSame[S, T, A, B] =
    new PSame[A, B, S, T] {
      def rsubst[R[_, _]](r: R[A, B]): R[S, T] =
        o(new OpticContext {
          type F[x]    = x
          type P[x, y] = R[x, y]
        })(r)
    }.flip

  implicit class SameOps[A, B](val s: PSame[A, A, B, B]) extends AnyVal {
    def subst[F[_]](fa: F[A]): F[B] = s.rsubst[λ[(s, t) => F[s]]](fa)
  }
}
