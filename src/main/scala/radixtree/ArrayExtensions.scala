package radixtree

import cats.kernel.{Eq, Hash}
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

extension [T](underlying: Array[T]) {

  private[radixtree] def updated(index: Int, value: T): Array[T] = {
    val result = underlying.clone
    result(index) = value
    result
  }

  private[radixtree] def patched(index: Int, value: T)(using ClassTag[T]): Array[T] = {
    val result = new Array[T](underlying.length + 1)
    System.arraycopy(underlying, 0, result, 0, index)
    result(index) = value
    if index < underlying.length then
      System.arraycopy(underlying, index, result, index + 1, underlying.length - index)
    result
  }

  private[radixtree] def resizeInPlace(n: Int)(using c: ClassTag[T]): Array[T] = {
    if underlying.length == n then
      underlying
    else {
      val r = c.newArray(n)
      System.arraycopy(underlying, 0, r, 0, n min underlying.length)
      r
    }
  }
}

private[radixtree] def arrayEqv[A: Eq](x: Array[A], y: Array[A]): Boolean = x.length == y.length && {
  var i = 0
  while i < x.length do {
    if !Eq.eqv(x(i), y(i)) then
      return false
    i += 1
  }
  true
}

private[radixtree] def arrayHash[A: Hash](a: Array[A]): Int = {
  var result = MurmurHash3.arraySeed
  var i = 0
  while i < a.length do {
    result = MurmurHash3.mix(result, Hash.hash(a(i)))
    i += 1
  }
  result
}

private[radixtree] given arrayEq[A: Eq]: Eq[Array[A]] with
  def eqv(x: Array[A], y: Array[A]): Boolean = arrayEqv[A](x, y)
private[radixtree] given arrayHash[A: Hash]: Hash[Array[A]] with
  def hash(x: Array[A]): Int = arrayHash[A](x)
  def eqv(x: Array[A], y: Array[A]): Boolean = arrayEqv[A](x, y)