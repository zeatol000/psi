/*     ____  _____ ____                         *\
 *    / __ \/ ___//  _/                         *
 *   / /_/ /\__ \ / /       Psi-lang 2026       *
 *  / ____/___/ // /                            *
 * /_/    /____/___/                            *
\*                                              *

package psic.tools
package collection

import psic.tools.*
import psic.tools.comp.util.Stats
// other stuff

import scala.annotation.{constructorOnly, tailrec}

import java.lang.ref.{ReferenceQueue, WeakReference}


object WeakHashSet {
  class Entry[A](
    @constructorOnly element: A,
    val hash: Int,
    var tail: Entry[A] | Null,
    @constructorOnly queue: ReferenceQueue[A]
  ) extends WeakReference[A](element, queue)

}


abstract class WeakHashSet[A <: AnyRef](initialCapacity: Int = 8, loadFactor: Double = 0.5) extends MutableSet[A] {
  import WeakHashSet.*

  type This = WeakHashSet[A]

  protected val queue = new ReferenceQueue[A]
  protected var count = 0

  private def computeCapacity: Int =
    if (initialCapacity < 0) throw new IllegalArgumentException("initial capacity cannot be less than 0")
    var candidate = 1
    while (candidate < initialCapacity) candidate *= 2
    candidate

  protected var table = new Array[Entry[A] | Null](computeCapacity)

  protected var threshold = computeThreshold

  private def computeThreshold: Int = (table.size * loadFactor).ceil.toInt

  protected def hash(key: A): Int
  protected def isEqual(x: A, y: A): Boolean = x.equals(y)

  protected def index(x: Int): Int = x & (table.length - 1)

  private def remove(bucket: Int, prevEntry: Entry[A] | Null, entry: Entry[A]): Unit = {
    Stats.record(statsItem("remove"))
    prevEntry match {
      case null => table(bucket) = entry.tail
      case _ => prevEntry.tail = entry.tail
    }
    count -= 1
  }

  protected def removeStaleEntries(): Unit = {
    def poll(): Entry[A] | Null = queue.poll().asInstanceOf

    @tailrec
    def queueLoop(): Unit = {
      val stale = poll()
      if (stale != null) {
        val bucket = index(stale.hash)

        @tailrec
        def linkedListLoop(prevEntry: Entry[A] | Null, entry: Entry[A] | Null): Unit =
          if entry != null then
            if stale eq entry then remove(bucket, prevEntry, entry)
            else linkedListLoop(entry, entry.tail)

        linkedListLoop(null, table(bucket))

        queueLoop()
      }
    }

    queueLoop()
  }

  protected def resize(): Unit = {
    Stats.record(statsItem("resize"))
    val oldTable = table
    table = new Array[Entry[A] | Null](oldTable.size * 2)
    threshold = computeThreshold

    @tailrec
    def tableLoop(oldBucket: Int): Unit = if (oldBucket < oldTable.size) {
      @tailrec
      def linkedListLoop(entry: Entry[A] | Null): Unit = entry match {
        case null => ()
        case _ =>
          val bucket = index(entry.hash)
          val oldNext = entry.tail
          entry.tail = table(bucket)
          table(bucket) = entry
          linkedListLoop(oldNext)
      }
      linkedListLoop(oldTable(oldBucket))

      tableLoop(oldBucket + 1)
    }
    tableLoop(0)
  }

  // TODO: remove the `case null` when we can enable explicit nulls in regular compiling,
  // since the type `A <: AnyRef` of `elem` can ensure the value is not null.
  def lookup(elem: A): A | Null = (elem: A | Null) match {
    case null => throw new NullPointerException("WeakHashSet cannot hold nulls")
    case _ =>
      Stats.record(statsItem("lookup"))
      removeStaleEntries()
      val bucket = index(hash(elem))

      @tailrec
      def linkedListLoop(entry: Entry[A] | Null): A | Null = entry match {
        case null                    => null
        case _                       =>
          val entryElem = entry.get
          if entryElem != null && isEqual(elem, entryElem) then entryElem
          else linkedListLoop(entry.tail)
      }

      linkedListLoop(table(bucket))
  }

  protected def addEntryAt(bucket: Int, elem: A, elemHash: Int, oldHead: Entry[A] | Null): A = {
    Stats.record(statsItem("addEntryAt"))
    table(bucket) = new Entry(elem, elemHash, oldHead, queue)
    count += 1
    if (count > threshold) resize()
    elem
  }

  // TODO: remove the `case null` when we can enable explicit nulls in regular compiling,
  // since the type `A <: AnyRef` of `elem` can ensure the value is not null.
  def put(elem: A): A = (elem: A | Null) match {
    case null => throw new NullPointerException("WeakHashSet cannot hold nulls")
    case _    =>
      Stats.record(statsItem("put"))
      removeStaleEntries()
      val h = hash(elem)
      val bucket = index(h)
      val oldHead = table(bucket)

      @tailrec
      def linkedListLoop(entry: Entry[A] | Null): A = entry match {
        case null                    => addEntryAt(bucket, elem, h, oldHead)
        case _                       =>
          val entryElem = entry.get
          if entryElem != null && isEqual(elem, entryElem) then entryElem.uncheckedNN
          else linkedListLoop(entry.tail)
      }

      linkedListLoop(oldHead)
  }

  def +=(elem: A): Unit = put(elem)

  def -=(elem: A): Unit = (elem: A | Null) match {
    case null =>
    case _ =>
      Stats.record(statsItem("-="))
      removeStaleEntries()
      val bucket = index(hash(elem))

      @tailrec
      def linkedListLoop(prevEntry: Entry[A] | Null, entry: Entry[A] | Null): Unit =
        if entry != null then
          val entryElem = entry.get
          if entryElem != null && isEqual(elem, entryElem) then remove(bucket, prevEntry, entry)
          else linkedListLoop(entry, entry.tail)

      linkedListLoop(null, table(bucket))
  }

  def clear(resetToInitial: Boolean): Unit = {
    table = new Array[Entry[A] | Null](table.size)
    threshold = computeThreshold
    count = 0

    @tailrec def queueLoop(): Unit = if (queue.poll() != null) queueLoop()
    queueLoop()
  }

  def size: Int = {
    removeStaleEntries()
    count
  }

  override def iterator: Iterator[A] = {
    removeStaleEntries()

    new collection.AbstractIterator[A] {

      private var currentBucket: Int = table.size

      private var entry: Entry[A] | Null = null

      private var lookaheadelement: A | Null = null

      @tailrec
      def hasNext: Boolean = {
        while (entry == null && currentBucket > 0) {
          currentBucket -= 1
          entry = table(currentBucket)
        }

        val e = entry
        if (e == null) false
        else {
          lookaheadelement = e.get
          if lookaheadelement == null then
            entry = e.tail
            hasNext
          else true
        }
      }

      def next(): A =
        if lookaheadelement == null then
          throw new IndexOutOfBoundsException("next on an empty iterator")
        else
          val result = lookaheadelement.nn
          lookaheadelement = null
          entry = entry.nn.tail
          result
    }
  }

  protected def statsItem(op: String): String = {
    val prefix = "WeakHashSet."
    val suffix = getClass.getSimpleName
    s"$prefix$op $suffix"
  }

  class Diagnostics {
    def fullyValidate(): Unit = {
      var computedCount = 0
      var bucket = 0
      while (bucket < table.size) {
        var entry = table(bucket)
        while (entry != null) {
          assert(entry.get != null, s"$entry had a null value indicated that gc activity was happening during diagnostic validation or that a null value was inserted")
          computedCount += 1
          val cachedHash = entry.hash
          val realHash = hash(entry.get.uncheckedNN)
          assert(cachedHash == realHash, s"for $entry cached hash was $cachedHash but should have been $realHash")
          val computedBucket = index(realHash)
          assert(computedBucket == bucket, s"for $entry the computed bucket was $computedBucket but should have been $bucket")

          entry = entry.tail
        }

        bucket += 1
      }

      assert(computedCount == count, s"The computed count was $computedCount but should have been $count")
    }

    def dump: String = java.util.Arrays.toString(table.asInstanceOf[Array[AnyRef | Null]])

    def collisionBucketsCount: Int =
      (table count (entry => entry != null && entry.tail != null))

    def fullBucketsCount: Int =
      (table count (entry => entry != null))

    def bucketsCount: Int = table.size
  }

  def diagnostics: Diagnostics = new Diagnostics

}*/
