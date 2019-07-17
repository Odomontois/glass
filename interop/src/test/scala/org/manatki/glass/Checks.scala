package org.manatki.glass

import scala.reflect.runtime.universe._
import interop._
import monocle.macros.{GenLens, GenPrism}

object Checks {
  val lolFoo: Lol Subset Foo   = GenPrism[Lol, Foo].toSubset
  val fooInt: Foo Contains Int = GenLens[Foo](_.x).toContains

  val ohShit = lolFoo andThen fooInt

  def valType[A: TypeTag](a: => A) = typeOf[A]

  ohShit: Lol Property Int
}

sealed trait Lol
final case class Foo(x: Int, s: String) extends Lol
