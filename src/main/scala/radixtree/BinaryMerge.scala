package radixtree

import Opts.*
import scala.annotation.tailrec

private[radixtree] final class BinaryMerge[V](val a: Array[RadixTree[V]], val b: Array[RadixTree[V]], val f: Option[(V, V) => Opt[V]], val r : Array[RadixTree[V]], var ri: Int) {

  private final def binarySearchB(ai: Int, b0: Int, b1: Int): Int = {
    @tailrec
    def binarySearch0(low: Int, high: Int): Int =
      if low <= high then {
        val mid = (low + high) >>> 1
        val c = compare(ai, mid)
        if c > 0 then
          binarySearch0(mid + 1, high)
        else if c < 0 then
          binarySearch0(low, mid - 1)
        else
          mid
      } else -(low + 1)
    binarySearch0(b0, b1 - 1)
  }

  inline def compare(ai: Int, bi: Int) = Key.compareAt(a(ai).prefix, 0, b(bi).prefix, 0)

  def collision(ai: Int, bi: Int): Unit = {
    r(ri) = a(ai).mergeWith(b(bi), f)
    ri += 1
  }

  def fromA(a0: Int, a1: Int, bi: Int): Unit = {
    System.arraycopy(a, a0, r, ri, a1 - a0)
    ri += a1 - a0
  }

  def fromB(ai: Int, b0: Int, b1: Int): Unit = {
    System.arraycopy(b, b0, r, ri, b1 - b0)
    ri += b1 - b0
  }

  def merge0(a0: Int, a1: Int, b0: Int, b1: Int): Unit = {
    if a0 == a1 then {
      if b0 != b1 then fromB(a0, b0, b1)
    } else if b0 == b1 then {
      fromA(a0, a1, b0)
    } else {
      val am = (a0 + a1) / 2
      val res = binarySearchB(am, b0, b1)
      if res >= 0 then {
        // same elements
        val bm = res
        // merge everything below a(am) with everything below the found element
        merge0(a0, am, b0, bm)
        // add the elements a(am) and b(bm)
        collision(am, bm)
        // merge everything above a(am) with everything above the found element
        merge0(am + 1, a1, bm + 1, b1)
      } else {
        val bm = -res - 1
        // merge everything below a(am) with everything below the found insertion point
        merge0(a0, am, b0, bm)
        // add a(am)
        fromA(am, am + 1, bm)
        // everything above a(am) with everything above the found insertion point
        merge0(am + 1, a1, bm, b1)
      }
    }
  }

}