package radixtree

object Opts {

  opaque type Opt[A] = A | Null

  object Opt {
    def apply[A](a: A): Opt[A] = a
    def empty[A]: Opt[A] = null
    def fromOption[A](a: Option[A]): Opt[A] = a match
      case Some(x) => x
      case _       => null
  }

  private given OptCanEqualNull[A]: CanEqual[Opt[A], Null] = CanEqual.derived

  extension [A](ref: Opt[A]) {
    def isDefined: Boolean = ref != null
    def isEmpty:   Boolean = ref == null

    def get: A = if ref == null then throw new NoSuchElementException("Opt.empty.get") else ref

    def ref: A | Null = ref

    def toOption: Option[A] = if ref == null then None else Some(ref)
  }

  extension[A](a: Opt[A]) def same(b: Opt[A]): Boolean =
    if a.isDefined && b.isDefined then a.get.equals(b.get)
    else a.isDefined == b.isDefined

  extension [A, B](ref: Opt[A])
    def map(f: A => B): Opt[B] = if ref == null then null else Opt.apply(f(ref))
    def flatMap(f: A => Opt[B]): Opt[B] = if ref == null then null else f(ref)

}