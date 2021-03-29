package sonicreducer

/**
 * An abstraction for a stateful reduce operation. The operation is called once for each element of the sequence to be
 * reduced, and then the result (if any) is retrieved using the result() method. Note that the result() operation is
 * side-effecting, and the reducer should no longer be used after calling result(). This is similar to a typical builder.
 * @tparam T the element and result type
 */
sealed abstract class Reducer[T] extends (T => Unit) {

  def result: T

  def resultOption(): Option[T]

  def resultOrElse(value: T): T

}

/**
 * A helper object to reduce collections using an associative operation and to produce stateful reducer objects.
 *
 * Instead of aggregating from the left to the right, the result will be aggregated
 * from the bottom to the top. E.g. for a sequence Array(1,2,3,4) and an operation (+), the reducers produced by this
 * helper class would execute (1 + 2) + (3 + 4) instead of (((1+2)+3)+4). This can have significant advantages when the
 * cost of an operation depends on the weight of an element.
 *
 * Consider string concatenation: concatenating a sequence of one-char strings of size N using seq.reduceLeft(_ + _)
 * would be an O(N**2) operation. Reducing it hierarchically would be an O(N*log(N)) operation.
 */
object Reducer {

  /**
   * Reduces an array hierarchically.
   * @param elements the elements to reduce
   * @param op the reduce operation. Must be associative
   * @tparam T the element type
   * @return an opt containing the result, or Opt.empty[T] if the array is of size 0
   */
  def reduceArray[T](elements: Array[T])(op: (T, T) => T): Option[T] = if elements.isEmpty then Option.empty[T] else {
    def m(a: Int, b: Int): Int = (a + b) / 2
    def reduce0(i0: Int, i1: Int): T = i1 - i0 match
      case 1 => elements(i0)
      case 2 => op(elements(i0), elements(i0 + 1))
      case _ =>
        val im = m(i0, i1)
        op(reduce0(i0, im), reduce0(im, i1))
    Option(reduce0(0, elements.length))
  }

  /**
   * Reduces any iterable collection hierarchically.
   * @param elements a iterable collection to be reduced
   * @param op the reduce operation. Must be associative
   * @tparam T the element and result type
   * @return an opt containing the result, or Opt.empty[T] if the collection is of size 0
   */
  def reduce[T](elements: TraversableOnce[T])(op: (T, T) => T): Option[T] = {
    val reducer = apply(op)
    elements.iterator.foreach(reducer)
    reducer.resultOption()
  }

  /**
   * Returns a stateful hierarchical reducer. This is used by the reduce operation internally. The reducer will perform
   * as many reduce steps as possible every time a new element is added. It will retain at most 32 non-reduced elements,
   * which will all be combined once the result() method is called.
   * @param op the reduce operation. Must be associative
   * @tparam T the element and result type
   * @return a stateful reducer that can be fed elements by calling apply
   */
  def apply[T](op: (T, T) => T): Reducer[T] = Impl[T](op)

  private[sonicreducer] type Nullable = AnyRef | Null

  private[sonicreducer] given NullableCanEqualNull: CanEqual[Nullable, Null] = CanEqual.derived

  private final class Impl[T](op: (T, T) => T) extends Reducer[T] {

    private var count = 0

    private val current = new Array[Nullable](32)

    inline private def combine(a: AnyRef, b: Nullable): Nullable =
      if b != null then op(a.asInstanceOf[T], b.asInstanceOf[T]).asInstanceOf[AnyRef]
      else a

    private def reduceTo(weight: Int, initial: Nullable = null): Nullable = {
      var i = 0
      var t = initial
      while i < weight do
        if current(i) != null then 
          t = combine(current(i).asInstanceOf[AnyRef], t)
          current(i) = null
        i += 1
      t
    }

    inline def apply(value: T): Unit = {
      count += 1
      val weight = Integer.numberOfTrailingZeros(count)
      current(weight) = reduceTo(weight, value.asInstanceOf[Nullable])
    }

    def result: T = {
      val result = reduceTo(32)
      if result != null then result.asInstanceOf[T] else throw new NoSuchElementException
    }

    def resultOrElse(default: T): T = {
      val result = reduceTo(32)
      if result == null then default else result.asInstanceOf[T]
    }

    def resultOption(): Option[T] = {
      count = 0
      Option(reduceTo(32)).asInstanceOf[Option[T]]
    }
    
  }

}