package radixtree

import Opts.*
import scala.annotation.tailrec

object Key:

  /**
    * The empty key
    */
  inline val empty = ""

  /**
    * The size of a key
    */
  inline def size(c: String): Int = c.length

  /**
    * An identity function for keys that can perform interning as an optimization
    */
  inline def intern(e: String): String = e

  inline def concat(a: String, b: String): String = a.concat(b).nn

  inline def slice(a: String, from: Int, until: Int): String = a.substring(from, until).nn

  /**
    * Compare key a at index ai with key b at index bi. This determines the order of keys in the tree
    */
  inline def compareAt(a: String, ai: Int, b: String, bi: Int): Int =  a.charAt(ai).compareTo(b.charAt(bi))

  inline def startsWith(a: String, b: String, ai: Int): Boolean = a.startsWith(b, ai)
  inline def indexOf(a: String, b: String): Int = a.indexOf(b)
  inline def regionMatches(a: String, ai: Int, b: String, bi: Int, count: Int): Boolean =
    a.regionMatches(ai, b, bi, count)

  
  inline def eqv(a: String, b: String): Boolean = a.equals(b)
  inline def hash(e: String): Int = scala.util.hashing.MurmurHash3.stringHash(e)

  /**
    * Starting from a at ai and b at bi, compares elements of a and b until count elements have been compared or until
    * a difference has been found.
    */
  @tailrec
  def indexOfFirstDifference(a: String, ai: Int, b: String, bi: Int, count: Int): Int = 
    if count == 0 || a(ai) != b(bi) then ai
    else indexOfFirstDifference(a, ai + 1, b, bi + 1, count - 1)

  final def binarySearch[V](elems: Array[RadixTree[V]], elem: String, offset: Int): Int = {
    @tailrec
    def binarySearch0(low: Int, high: Int): Int =
      if low <= high then {
        val mid = (low + high) >>> 1
        val c = compareAt(elem, offset, elems(mid).prefix, 0)
        if c > 0 then
          binarySearch0(mid + 1, high)
        else if c < 0 then
          binarySearch0(low, mid - 1)
        else
          mid
      } else -(low + 1)
    binarySearch0(0, elems.length - 1)
  }

  final def mergeChildren[V](a: Array[RadixTree[V]], b: Array[RadixTree[V]], f: Option[(V, V) => Opt[V]]): Array[RadixTree[V]] = {
    val r = new Array[RadixTree[V]](a.length + b.length)
    val bm = new BinaryMerge(a, b, f, r, 0)
    bm.merge0(0, a.length, 0, b.length)
    r.resizeInPlace(bm.ri)
  }
