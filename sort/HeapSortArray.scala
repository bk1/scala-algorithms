/** -*- coding: utf-8-unix -*- Юникод/UTF-8
 *
 * Implements an example of Heap sort.
 *
 * In English, here's how the algorithm works:
 * Push all elements onto a max heap (in place). Then pop off the root node (ie, max 
 * remaining value on heap) until you've popped all values off the heap. The values
 * leave the heap in reverse sorted order, so as the heap shrinks, we can put popped
 * values to the right end of the same array.
 */

import scala.util.Random

object HeapSortArray {
  def main(args: Array[String]): Unit = {

    val hs1 : HeapSortArray[Int] = new HeapSortArray();
    val mess0 : Array[Int] = Array.fill(10)(Math.abs(Random.nextInt));
    hs1.sort(mess0)
    mess0.foreach( println )
    println

    val mess1 = Array(3, 9, 8, 13, 2, 5, 4);
    hs1.sort(mess1)
    mess1.foreach( println )
    println

    val hs2 : HeapSortArray[String] = new HeapSortArray();
    val mess2 = Array("3", "x9", "y8", "и13", "ü2", "ö5", "ярпыж4");
    hs2.sort(mess2)
    mess2.foreach( println )
    println
  }
}

class HeapSortArray[T : Ordering] {
 
  private def parentIdx(idx : Int) : Int = { (idx - 1) / 2 }

  private def leftChildIdx(idx : Int) : Int = { 2*idx + 1 }
  
  private def rightChildIdx(idx : Int) : Int = { 2*idx + 2 }

  private def siftDown(arr : Array[T], start : Int)(implicit ordering : Ordering[T]) : Array[T] = {
    var root = start
    var leftChild = leftChildIdx(root)
    var rightChild = rightChildIdx(root)
    while (leftChild < arr.length) {
      var other = root
      if (ordering.compare(arr(other), arr(leftChild)) < 0) {
        other = leftChild
      }
      if (rightChild < arr.length && ordering.compare(arr(other), arr(rightChild)) < 0) {
        other = rightChild
      }
      if (other != root) {
        swap(arr, root, other)
        root = other
        leftChild = leftChildIdx(root)
        rightChild = rightChildIdx(root)
      } else {
        return arr
      }
    }
    return arr
  }


  /**Pushes an illegally located element down the heap to restore heap property.*/
  @annotation.tailrec
  private def heapify(arr: Array[T], loc: Int, lastLeaf: Int)(implicit ordering : Ordering[T]) : Unit = {
    val l = leftChildIdx(loc) 
    val r = rightChildIdx(loc)

    var max = loc

    if (l <= lastLeaf && ordering.compare(arr(l), arr(max)) > 0) { max = l }
    if (r <= lastLeaf && ordering.compare(arr(r), arr(max)) > 0) { max = r }

    if (max != loc) {
      swap(arr, max, loc)
      heapify(arr, max, lastLeaf)(ordering)
    }
  }
    

  def sort(a: Array[T])(implicit ordering : Ordering[T]): Unit = {
    var m = a.length - 1 
    buildHeap(a, m)(ordering)
    while (m >= 1) {
      swap(a, 0, m)
      m-=1
      heapify(a, 0, m)(ordering)
    }
  }

  private def buildHeap(a: Array[T], m: Int)(implicit ordering : Ordering[T]): Unit = {
    for (i <- m/2 to 0 by -1) {
      heapify(a, i, m)(ordering)
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

  private def swap(a: Array[T], i: Int, j:Int): Unit = {
    val staging = a(i)
    a(i) = a(j)
    a(j) = staging
  }
}
