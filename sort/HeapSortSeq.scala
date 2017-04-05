/** -*- coding: utf-8-unix -*- Юникод/UTF-8
 *
 * Implements an example of Heap sort.
 *
 * In English, here's how the algorithm works:
 * Push all elements onto a max heap (in place). Then pop off the root node (ie, max 
 * remaining value on heap) until you've popped all values off the heap. The values
 * leave the heap in reverse sorted order, so as the heap shrinks, we can put popped
 * values to the right end of the same seqay.
 */

import scala.collection.immutable.Vector
import scala.collection.immutable.Seq
import scala.util.Random

object HeapSortSeq {
  def main(args: Array[String]): Unit = {

    val hs1 : HeapSortSeq[Int] = new HeapSortSeq();
    val mess0 : Seq[Int] = Seq.fill(10)(Math.abs(Random.nextInt));
    hs1.sort(mess0)
    mess0.foreach( println )
    println

    val mess1 : Seq[Int] = Seq(3, 9, 8, 13, 2, 5, 4);
    hs1.sort(mess1)
    mess1.foreach( println )
    println

    val hs2 : HeapSortSeq[String] = new HeapSortSeq();
    val mess2 = Seq("3", "x9", "y8", "и13", "ü2", "ö5", "ярпыж4");
    hs2.sort(mess2)
    mess2.foreach( println )
    println
  }
}

class HeapSortSeq[T : Ordering] {

  def sort(a: Seq[T])(implicit ordering : Ordering[T]): Seq[T]  = {
    var seq = a
    var m = seq.length - 1 
    buildHeap(a, m)(ordering)
    while (m >= 1) {
      seq = swap(seq, 0, m)
      m-=1
      seq = heapify(seq, 0, m)(ordering)
    }
    return seq;
  }

  private def buildHeap(a: Seq[T], m: Int)(implicit ordering : Ordering[T]): Unit = {
    var seq = a
    for (i <- m/2 to 0 by -1) {
      seq = heapify(seq, i, m)(ordering)
    }
  }

  /**Pushes an illegally located element down the heap to restore heap property.*/
  @annotation.tailrec
  private def heapify(a: Seq[T], loc: Int, lastLeaf: Int)(implicit ordering : Ordering[T]) : Seq[T] = {
    var seq = a
    val l = left(loc) 
    val r = right(loc)

    var max = loc

    if (l <= lastLeaf && ordering.compare(seq(l), seq(max)) > 0) { max = l }
    if (r <= lastLeaf && ordering.compare(seq(r), seq(max)) > 0) { max = r }

    if (max != loc) {
      seq = swap(seq, max, loc)
      heapify(seq, max, lastLeaf)(ordering)
    } else {
      seq
    }
  }

  /**Returns position of left child (possibly empty). */
  private def left(loc: Int): Int = {
    return 2*loc
  }

  /**Returns position of right child (possibly empty). */
  private def right(loc: Int): Int = {
    return 2*loc+1
  }

  private def swap(seq: Seq[T], i: Int, j:Int): Seq[T] = {
    val ai = seq(i)
    val aj = seq(j)
    seq.patch(i, Seq(aj), 1).patch(j, Seq(ai), 1)
  }
}
