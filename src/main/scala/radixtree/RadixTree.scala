package radixtree

import cats.*
import scala.util.hashing.MurmurHash3
import Opts.*
import Key.*
import sonicreducer.{Reducer, given}
import scala.collection.Iterable
import Memo.*
import scala.annotation.tailrec

final class RadixTree[V](val prefix: String, private[radixtree] val children: Array[RadixTree[V]], private[radixtree] val valueOpt: Opt[V]) { self =>

  def count: Int = {
    var n = if valueOpt.isDefined then 1 else 0
    var i = 0
    while i < children.length do {
      n += children(i).count
      i += 1
    }
    n
  }

  def printStructure: String = children.mkString(s"RadixTree($prefix, $valueOpt, [", ",", "])")

  inline def isEmpty = Key.size(prefix) == 0

  def prepend(prefix: String): RadixTree[V] = new RadixTree[V](Key.concat(prefix, this.prefix), children, valueOpt)

  inline def startsWith(prefix: String) = filterPrefix(prefix) eq this

  inline def filterPrefix(prefix: String): RadixTree[V] = filterPrefix0(prefix, 0)

  def filterPrefixesOf(query: String): RadixTree[V] = {
    val ft = filterPrefixesOf0(this, query, 0)
    if ft.prefix == Key.empty then
      if !ft.children.isEmpty then ft.children.head
      else RadixTree.empty
    else ft
  }

  def subtreeWithPrefix(prefix: String): RadixTree[V] = {
    val tree1 = filterPrefix(prefix)
    if Key.startsWith(tree1.prefix, prefix, 0) then
      tree1.copy(prefix = Key.slice(tree1.prefix, Key.size(prefix), Key.size(tree1.prefix)))
    else
      RadixTree.empty
  }

  private def subtrees = new Iterator[(String, Opt[V])] {
    private var path: List[(RadixTree[V], Int, String)] = List()
    private var tree: RadixTree[V] = self
    private var i: Int = -1
    private var prefix: String = self.prefix
    override def hasNext: Boolean = i < tree.children.length ||
       path.exists{ (tree, i, k) => i < tree.children.length }
    @tailrec
    override def next: (String, Opt[V]) = 
      if i == -1 then // Ensure we return the root element
        i = 0
        (tree.prefix, tree.valueOpt)
      else if i < tree.children.length then
        val child = tree.children(i)
        val childPrefix = prefix.concat(child.prefix).nn
        if child.children.length != 0 then // Non-leaf element, proceed down into it after pushing the current values
          path = (tree, i + 1, prefix) :: path // i + 1 so we go to the next child when returning to this level
          tree = child; i = 0; prefix = childPrefix
        else // If this is a leaf element, don't bother going down into it, since we'd just jump back out immediately
          i = i + 1
        (childPrefix, child.valueOpt)
      else if path.nonEmpty then // Pop the previous level and continue from where we left off
        val (prevTree, prevI, prevPrefix) = path.head
        path = path.tail 
        tree = prevTree
        i = prevI
        prefix = prevPrefix
        next
      else throw new NoSuchElementException("Iterator is exhausted")
  }

  def entries: Iterable[(String, V)] = new Iterable[(String, V)] {
    def iterator = subtrees.filter(_._2.isDefined).map((key, valueOpt) => (key, valueOpt.get))
    override def foreach[U](f: ((String, V)) => U) = foreachEntry(Key.empty, f)
  }

  def values: Iterable[V] = new Iterable[V] {
    def iterator = subtrees.filter(_._2.isDefined).map(_._2.get)
    override def foreach[U](f: V => U) = foreachValue(f)
  }

  def keys: Iterable[String] = new Iterable[String] {
    def iterator = subtrees.filter(_._2.isDefined).map(_._1)
    override def foreach[U](f: String => U) = foreachKey(Key.empty, f)
  }

  private def foreachChild[U](f: RadixTree[V] => U): Unit = {
    var i = 0
    while i < children.length do {
      f(children(i))
      i += 1
    }
  }

  private def foreachEntry[U](prefix: String, f: ((String, V)) => U): Unit = {
    val newPrefix = prefix.concat(this.prefix).nn
    if valueOpt.isDefined then
      f((newPrefix, valueOpt.get))
    foreachChild(_.foreachEntry(newPrefix, f))
  }

  private def foreachValue[U](f: V => U): Unit = {
    if valueOpt.isDefined then
      f(valueOpt.get)
    foreachChild(_.foreachValue(f))
  }

  private def foreachKey[U](prefix: String, f: String => U): Unit = {
    val newPrefix = prefix.concat(this.prefix).nn
    if valueOpt.isDefined then
      f(newPrefix)
    foreachChild(_.foreachKey(newPrefix, f))
  }

  private def filterPrefix0(pre: String, offset: Int): RadixTree[V] = {
    val ps = Key.size(prefix)
    val pres = Key.size(pre)
    val maxFd = ps.min(pres - offset)
    val fd = Key.indexOfFirstDifference(prefix, 0, pre, offset, maxFd)
    if fd == maxFd then {
      if maxFd < ps || pres - offset == ps then
        this
      else {
        val index = Key.binarySearch(children, pre, offset + ps)
        if index >= 0 then {
          val child1 = children(index).filterPrefix0(pre, offset + ps)
          val children1 =
            if child1.isEmpty then RadixTree.emptyChildren[V]
            else Array(child1)
          copy(valueOpt = Opt.empty[V], children = children1)
        } else
          RadixTree.empty
      }
    } else
      RadixTree.empty
  }

  private def filterPrefixesOf0(acc: RadixTree[V], query: String, offset: Int): RadixTree[V] = {
    val ps = Key.size(prefix)
    val qs = Key.size(query) - offset
    val maxFd = ps.min(qs)
    val fd = Key.indexOfFirstDifference(prefix, 0, query, offset, maxFd)
    if fd == maxFd then {
      if maxFd < ps then RadixTree.empty
      else if qs == ps then copy(children = RadixTree.emptyChildren)
      else {
        val index = Key.binarySearch(children, query, offset + ps)
        if index >= 0 then {
          val child1 = children(index).filterPrefixesOf0(this, query, offset + ps)
          val children1 = if child1.isEmpty then RadixTree.emptyChildren[V] else Array(child1)
          copy(children = children1)
        } else {
          copy(children = RadixTree.emptyChildren)
        }
      }
    } else RadixTree.empty
  }

  def modifyOrRemove(f: (String, V, Int) => Option[V]): RadixTree[V] =
    modifyOrRemove0(f, Key.empty)

  private def modifyOrRemove0(f: (String, V, Int) => Option[V], prefix: String): RadixTree[V] = {
    val newPrefix = Key.concat(prefix, this.prefix)
    val builder = Array.newBuilder[RadixTree[V]]
    builder.sizeHint(children.length)
    for (child <- children) {
      val child1 = child.modifyOrRemove0(f, newPrefix)
      if !child1.isEmpty then
        builder += child1
    }
    val temp = builder.result()
    val children1 =
      if children.length == temp.length && children.corresponds(temp)(_ eq _) then children
      else if temp.isEmpty then RadixTree.emptyChildren[V]
      else temp
    val valueOpt1: Opt[V] = if valueOpt.isDefined then Opt.fromOption(f(newPrefix, valueOpt.get, children1.length)) else Opt.empty
    copy(children = children1, valueOpt = valueOpt1)
  }

  def mapValues[V2](f: V ⇒ V2): RadixTree[V2] =
    new RadixTree[V2](prefix, children.map(_.mapValues(f)), valueOpt.map(f))

  def filter(f: (String, V) => Boolean): RadixTree[V] =
    filter0(f, Key.empty)

  private def filter0(f: (String, V) => Boolean, prefix: String): RadixTree[V] = {
    val prefix1 = Key.concat(prefix, this.prefix)
    val builder = Array.newBuilder[RadixTree[V]]
    builder.sizeHint(children.length)
    for (child <- children) {
      val child1 = child.filter0(f, prefix1)
      if !child1.isEmpty then
        builder += child1
    }
    val temp = builder.result()
    val children1 =
      if children.length == temp.length && children.corresponds(temp)(_ eq _) then children
      else if temp.isEmpty then RadixTree.emptyChildren[V]
      else temp
    val newValueOpt = if valueOpt.isDefined && f(prefix1, valueOpt.get) then valueOpt else Opt.empty
    copy(children = children1, valueOpt = newValueOpt)
  }

  private def copy(
      prefix: String = this.prefix,
      valueOpt: Opt[V] = this.valueOpt,
      children: Array[RadixTree[V]] = this.children
    ): RadixTree[V] = {
    def same(a: Opt[V], b: Opt[V]): Boolean =
      if a.isDefined && b.isDefined then
        a.get.asInstanceOf[AnyRef] eq b.get.asInstanceOf[AnyRef]
      else a.isDefined == b.isDefined
    if Key.eqv(prefix, this.prefix) && same(valueOpt, this.valueOpt) && ((children eq this.children) || (children.length == 0 && this.children.length == 0)) then
      this
    else if valueOpt.isEmpty then
      children.length match {
        case 0 => RadixTree.empty
        case 1 => children(0).prepend(prefix)
        case _ => new RadixTree[V](Key.intern(prefix), children, valueOpt)
      }
    else
      new RadixTree[V](Key.intern(prefix), children, valueOpt)
  }

  def merge(other: RadixTree[V]): RadixTree[V] =
    merge0(other, 0, None)

  def mergeWith(other: RadixTree[V], collision: (V, V) => Opt[V]): RadixTree[V] =
    merge0(other, 0, Some(collision))

  def mergeWith(other: RadixTree[V], collision: Option[(V, V) => Opt[V]]): RadixTree[V] =
    merge0(other, 0, collision)

  def apply(key: String) = get0(key, 0).get

  def contains(key: String) = get0(key, 0).isDefined

  def get(key: String): Option[V] = get0(key, 0).toOption

  def getOrNull(key: String): V | Null = get0(key, 0).ref

  def getOrDefault[VV >: V](key: String, default: VV): VV = {
    val o = get0(key, 0)
    if o.isDefined then o.get else default
  }

  @tailrec
  private def get0(key: String, offset: Int): Opt[V] =
    if Key.startsWith(key, prefix, offset) then {
      val newOffset = offset + Key.size(prefix)
      if Key.size(key) == newOffset then valueOpt
      else {
        val index = Key.binarySearch(children, key, newOffset)
        if index >= 0 then children(index).get0(key, newOffset)
        else Opt.empty
      }
    } else
      Opt.empty

  private def merge0(that: RadixTree[V], offset: Int, collision: Option[(V, V) => Opt[V]]): RadixTree[V] = {
    val ps = Key.size(prefix)
    val tps = Key.size(that.prefix)
    val tps1 = tps - offset
    val maxFd = Math.min(ps,tps1)
    val fd = Key.indexOfFirstDifference(prefix, 0, that.prefix, offset, maxFd)
    if fd == maxFd then {
      // prefixes match
      if maxFd < ps then {
        // this.prefix is longer than (that.prefix.size - offset)
        val prefix0 = Key.slice(prefix, 0, fd)
        val prefix1 = Key.slice(prefix, fd, ps)
        val this1 = copy(prefix = prefix1)
        val children1 = Key.mergeChildren(Array(this1), that.children, collision)
        copy(prefix = prefix0, valueOpt = that.valueOpt, children = children1)
      } else if tps1 == ps then {
        // this.prefix is the same as other.prefix when adjusted by offset
        // merge the values and children using the collision function if necessary
        val mergedValueOpt = if this.valueOpt.isDefined then
          if collision.isDefined && that.valueOpt.isDefined then
            collision.get.apply(this.valueOpt.get, that.valueOpt.get)
          else
            this.valueOpt
        else
          that.valueOpt
        new RadixTree[V](this.prefix, Key.mergeChildren(this.children, that.children, collision), mergedValueOpt)
      } else {
        val childOffset = offset + Key.size(prefix)
        val index = Key.binarySearch(children, that.prefix, childOffset)
        val children1 = if index >= 0 then {
          val child1 = children(index).merge0(that, childOffset, collision)
          children.updated(index, child1)
        } else {
          val tp1 = Key.slice(that.prefix, childOffset, tps)
          val child1 = that.copy(prefix = tp1)
          children.patched(-index - 1, child1)
        }
        copy(children = children1)
      }
    } else {
      // both trees have a common prefix (might be "")
      val commonPrefix = Key.slice(prefix, 0, fd)
      val p1 = Key.slice(this.prefix, fd, ps)
      val tp1 = Key.slice(that.prefix, offset + fd, tps)
      val childA = this.copy(prefix = p1)
      val childB = that.copy(prefix = tp1)
      val children1 =
        if Key.compareAt(childA.prefix, 0, childB.prefix, 0) < 0 then
          Array(childA, childB)
        else
          Array(childB, childA)
      new RadixTree[V](commonPrefix, children1, Opt.empty)
    }
  }

  def filterKeysContaining(fragment: String)(using v: Hash[V]) = {
    lazy val filter = Memo.fromFunction[RadixTree[V]](tree ⇒ filter0(Key.empty, tree))(using RadixTree.RadixTreeHashEqv)
    def filter0(prefix: String, tree: RadixTree[V]): RadixTree[V] = {
      val prefix1 = Key.concat(prefix, tree.prefix)
      if Key.indexOf(prefix1, fragment) >= 0 then tree
      else {
        val p1s = Key.size(prefix1)
        val fs = Key.size(fragment)
        val children1 = tree.children.flatMap { child =>
          val prefixEnd = Key.slice(prefix1, (p1s - fs + 1) max 0, p1s)
          val pes = Key.size(prefixEnd)
          var result = filter(child)
          for (i <- 1 until (fs min (pes + 1))) {
            if Key.regionMatches(fragment, 0, prefixEnd, pes - i, i) then
              result = result.merge(child.filterPrefix(Key.slice(fragment, i, fs)))
          }
          if result.isEmpty then None else Some(result)
        }
        val children2 =
          if children1.isEmpty then RadixTree.emptyChildren[V]
          else children1
        tree.copy(valueOpt = Opt.empty[V], children = children2)
      }
    }
    filter0(Key.empty, this)
  }

  override def equals(a: Any): Boolean = throw new UnsupportedOperationException("RadixTree.equals is not supported, use Eqv[RadixTree[V]].eq instead")

  override def hashCode: Int = throw new UnsupportedOperationException("RadixTree.hashCode is not supported, use Hash[RadixTree[V]].hash instead")

  override def toString: String = entries
    .map { (k, v) ⇒ s"$k->$v" }
    .mkString("RadixTree(", ",", ")")

}

object RadixTree {

  inline def empty[V]: RadixTree[V] = new RadixTree[V](Key.empty, emptyChildren[V], Opt.empty)

  inline def singleton[V](key: String, value: V): RadixTree[V] =
    new RadixTree[V](key, RadixTree.emptyChildren[V], Opt(value))

  def apply[V](kvs: (String, V)*): RadixTree[V] = {
    val reducer = Reducer[RadixTree[V]](_.merge(_))
    for ((k, v) <- kvs)
      reducer.apply(singleton(k, v))
    reducer.resultOrElse(empty[V])
  }

  private def emptyChildren[V]: Array[RadixTree[V]] = _emptyChildren.asInstanceOf[Array[RadixTree[V]]]

  private val _emptyChildren = Array.empty[RadixTree[?]]

  given RadixTreeEqv[V: Eq]: Eq[RadixTree[V]] with {
    def eqv(x: RadixTree[V], y: RadixTree[V]) = {
      def same(a: Opt[V], b: Opt[V]): Boolean =
        if a.isDefined && b.isDefined then
          Eq.eqv(a.get, b.get)
        else a.isDefined == b.isDefined
      Eq.eqv(x.prefix, y.prefix) &&
      same(x.valueOpt, y.valueOpt) &&
      arrayEqv(x.children, y.children)
    }
  }

  given RadixTreeHashEqv[V: Hash]: Hash[RadixTree[V]] with {
    def eqv(x: RadixTree[V], y: RadixTree[V]) = RadixTreeEqv.eqv(x, y)
    def hash(a: RadixTree[V]): Int = {
      val valueHash = if a.valueOpt.isDefined then Hash.hash(a.valueOpt.get) else 0
      val prefixHash = Hash.hash(a.prefix)
      val childrenHash = arrayHash(a.children)
      MurmurHash3.mixLast(MurmurHash3.mix(prefixHash, valueHash), childrenHash)
    }
  }

  given RadixTreeShow[V: Show]: Show[RadixTree[V]] = Show.show {
    _.entries
      .map { (k, v) ⇒ s"$k->$v" }
      .mkString("RadixTree(", ",", ")")
  }


  given RadixTreeMonoid[A](using m: Monoid[A]): Monoid[RadixTree[A]] with
    def empty: RadixTree[A] = RadixTree.empty
    def combine(x: RadixTree[A], y: RadixTree[A]): RadixTree[A] = x.mergeWith(y, (x, y) => Opt(m.combine(x, y)))

}