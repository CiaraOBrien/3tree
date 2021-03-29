package radixtree

import org.scalatest.funsuite.AnyFunSuite
import scala.language.adhocExtensions
import cats.kernel.{Eq, Hash, Monoid}
import Opts.*
import Key.*

class Radix

class RadixTreeTest extends AnyFunSuite {

  private given NullableCanEqualNull[A]: CanEqual[A | Null, Null] = CanEqual.derived

  val kvs = 0.until(100).map(i => i.toString -> i)

  val kvs1k = 0.until(1000).map(i => i.toString -> i)

  val tree = RadixTree(kvs*)

  val tree1k = RadixTree(kvs1k*)

  val textkvs = 0.until(1000).map(x => NumberToWord(x) -> x)

  val texttree = RadixTree(textkvs*)

  def testCreate[V: Eq](kvs: (String, V)*): Unit = {
    val tree = RadixTree(kvs*)
    assert(kvs.size === tree.count)
    for ((k, v) <- kvs) {
      assert(tree.contains(k))
      assert(tree.get(k).isDefined)
      assert(Eq.eqv(tree(k), v))
    }
  }

  def testEquals[V: Eq](kvs: (String, V)*): Unit = {
    assert(Eq.eqv(RadixTree(kvs*), RadixTree(kvs.reverse*)))
  }

  def testHashCode[V: Hash](kvs: (String, V)*): Unit = {
    assert(Hash.hash(RadixTree(kvs*)) === Hash.hash(RadixTree(kvs.reverse*)))
  }

  def testGeneric[V: Hash](kvs: (String, V)*): Unit = {
    testCreate(kvs*)
    testHashCode(kvs*)
    testEquals(kvs*)
  }

  test("generic") {
    testGeneric(kvs*)
    testGeneric(kvs1k*)
    testGeneric(textkvs*)
  }

  test("equals") {
    intercept[UnsupportedOperationException] {
      tree.equals("foo")
    }
    assert(Eq.eqv(tree, RadixTree(kvs*)))
    assert(Eq.eqv(tree, RadixTree(kvs.reverse*)))
  }

  test("hashCode") {
    intercept[UnsupportedOperationException] {
      tree.hashCode
    }
    assert(Hash.hash(tree) === Hash.hash(RadixTree(kvs*)))
  }

  test("mapValues") {
    assert(Eq.eqv(RadixTree("1" -> 1, "11" -> 11).mapValues(_.toString), RadixTree("1" -> "1", "11" -> "11")))
  }

  test("toString") {
    import cats.implicits.*
    assert(!RadixTree("1" -> 1).toString.isEmpty)
    assert(!RadixTree("1" -> 1).printStructure.isEmpty)
    assert(!RadixTree("1" -> 1).show.isEmpty)
  }

  test("startsWith") {
    assert(RadixTree("1" -> 1).startsWith("1"))
    assert(RadixTree("11" -> 1).startsWith("1"))
    assert(!RadixTree("11" -> 1).startsWith("2"))
    assert(!RadixTree("1" -> 1).startsWith("11"))
  }

  test("emptyIsEmpty") {
    assert(RadixTree.empty[Int].isEmpty)
    assert(RadixTree.empty[String].isEmpty)
  }

  test("contains") {
    assert(kvs.size === tree.count)
    for (i <- 0.until(100)) assert(i === tree(i.toString))
    assert(!tree.contains("101"))
    assert(!tree.contains("-1"))
    assert(!RadixTree("a" -> 1).contains("b"))
  }

  test("mergeNoCollision") {
    val a = RadixTree("a" -> 1)
    val b = a.mergeWith(a, (x, y) => Opt(x + y))
    assert(2 === b("a"))
  }

  test("entries") { 
    assert(kvs.toSet === tree.entries.toSet)
    assert(Eq[Array[(String, Int)]].eqv(tree.entries.toArray, RadixTree(kvs*).entries.toArray))
  }

  test("keys") {
    assert(kvs.map(_._1).toSet === tree.keys.toSet)
  }

  test("values") {
    assert(kvs.map(_._2).toSet === tree.values.toSet)
  }

  test("filterPrefix") {
    assert(kvs.filter { (k, v) => k.startsWith("1") }.toSeq === tree.filterPrefix("1").entries.toSeq)
    assert(RadixTree("1" -> 1).filterPrefix("foo").isEmpty)
    assert(RadixTree("1" -> 1, "12" -> 12).filterPrefix("123").isEmpty)
  }

  test("filterPrefixesOf") {
    assert(RadixTree("1" -> 1).entries.toSeq === tree.filterPrefixesOf("1x").entries.toSeq)
    assert(RadixTree("1" -> 1).filterPrefixesOf("foo").isEmpty)
    assert(RadixTree("1" -> 1, "12" -> 12).filterPrefixesOf("2").isEmpty)
  }

  test("modifyOrRemove") { 
    val tree1 = tree.modifyOrRemove { (k, v, _) => Some(v * 2) }
    val tree2 = tree.modifyOrRemove { (k, v, _) => None }
    for ((k, v) <- kvs)
      assert(v * 2 === tree1(k))
    assert(tree2.isEmpty)
  }

  test("subtreeWithPrefix") {
    assert(tree.subtreeWithPrefix("x").isEmpty)
    assert(11 === tree.subtreeWithPrefix("1").count)
  }

  test("filter") {
    assert(kvs.filter { (k, v) => k.startsWith("1") }.toSeq === 
          tree.filter { (k, v) => k.startsWith("1") }.entries.toSeq)
  }

  test("filterKeysContaining1") {
    val tt = RadixTree("aa" -> 1, "ab" -> 2, "a" -> 3)
    assert(1 === tt.filterKeysContaining("aa").count)
    assert(0 === tt.filterKeysContaining("aaa").count)
    val t = RadixTree("a" -> 1)
    assert(t.filterKeysContaining("b").isEmpty)
    assert(!t.filterKeysContaining("a").isEmpty)
    assert(
      kvs1k.count(_._1.contains("10")) ===
      tree1k.filterKeysContaining("10").count
    )
    assert(
      kvs.count(_._1.contains("1")) ===
      tree.filterKeysContaining("1").count
    )
    assert(
      textkvs.count(_._1.contains("eight")) ===
      texttree.filterKeysContaining("eight").count
    )
  }

  test("filterKeysContaining2") {
    assert(
      kvs1k.count(_._1.contains("10")) ===
        tree1k.filterKeysContaining("10").count
    )
    assert(1 === RadixTree("abcd" -> 1).filterKeysContaining("bcd").count)
  }

  test("opt") {
    intercept[NoSuchElementException] {
      Opt.empty[Int].get
    }
    assert(Opt.empty[Int].toOption.isEmpty)
  }

  test("getOrNull") {
    val t = RadixTree.singleton("ab", "x")
    assert(t.getOrNull("ab").equals("x"))
    assert(t.getOrNull("ba") == null)
  }

  test("getOrDefault") {
    val t = RadixTree.singleton("ab", 3)
    assert(t.getOrDefault("ab", 7) == 3)
    assert(t.getOrDefault("ba", 7) == 7)
  }

  test("eq") {
    Eq.eqv(tree, RadixTree(kvs*))
  }

  test("monoid") {
    val entries = textkvs.map { (k, v) => (k, ()) }
    val singles = entries.map { (k, v) => RadixTree.singleton(k, v) }
    assert(Eq.eqv(RadixTree(entries*), Monoid[RadixTree[Unit]].combineAll(singles)))
  }

  test("wordCount") {
    val m = summon[Monoid[RadixTree[Int]]]
    val words = textkvs.flatMap((k, v) ⇒ k.split(' ').nn.map(k => RadixTree.singleton(k, 1))).reduce(m.combine)
    assert(words.count === 101)
  }

  test("arrayEq") {
    assert(!arrayEqv(Array(1,2), Array(1,3)))
  }

}