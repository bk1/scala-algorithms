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

object HeapSortArraySegmented {
  def main(args: Array[String]): Unit = {

    val hs1 : HeapSortArraySegmented[Int] = new HeapSortArraySegmented();
    val mess0 : Array[Int] = Array.fill(10)(Math.abs(Random.nextInt));
    hs1.sort(mess0)
    mess0.foreach( println )
    println

    val mess1 = Array(3, 9, 8, 13, 2, 5, 4);
    hs1.sort(mess1)
    mess1.foreach( println )
    println

    val hs2 : HeapSortArraySegmented[String] = new HeapSortArraySegmented();
    val mess2 = Array("3", "x9", "y8", "и13", "ü2", "ö5", "ярпыж4");
    hs2.sort(mess2)
    mess2.foreach( println )
    println
  }
}

class HeapSortArraySegmented[T : Ordering] {
 
  private def parentIdx(idx : Int, base : Int) : Int = { (idx - base - 1) / 2 + base }

  private def leftChildIdx(idx : Int, base : Int) : Int = { 2*(idx - base) + 1 + base }
  
  private def rightChildIdx(idx : Int, base : Int) : Int = { 2*(idx - base) + 2 + base }

  private def siftDown(arr : Array[T], start : Int, end : Int, base : Int)(implicit ordering : Ordering[T]) : Array[T] = {
    var root = start
    var leftChild = leftChildIdx(root, base)
    var rightChild = rightChildIdx(root, base)
    while (leftChild < end) {
      var other = root
      if (ordering.compare(arr(other), arr(leftChild)) < 0) {
        other = leftChild
      }
      if (rightChild < end && ordering.compare(arr(other), arr(rightChild)) < 0) {
        other = rightChild
      }
      if (other != root) {
        swap(arr, root, other)
        root = other
        leftChild = leftChildIdx(root, base)
        rightChild = rightChildIdx(root, base)
      } else {
        return arr
      }
    }
    return arr
  }


  /**Pushes an illegally located element down the heap to restore heap property.*/
  @annotation.tailrec
  private def heapify(arr: Array[T], loc: Int, lastLeaf: Int, base : Int)(implicit ordering : Ordering[T]) : Unit = {
    val l = leftChildIdx(loc, base) 
    val r = rightChildIdx(loc, base)

    var max = loc

    if (l <= lastLeaf && ordering.compare(arr(l), arr(max)) > 0) { max = l }
    if (r <= lastLeaf && ordering.compare(arr(r), arr(max)) > 0) { max = r }

    if (max != loc) {
      swap(arr, max, loc)
      heapify(arr, max, lastLeaf, base)(ordering)
    }
  }
    

  def sort(a: Array[T])(implicit ordering : Ordering[T]): Array[T] = {
    sortSegmented(a, 0, a.length)(ordering)
  }

  def sortSegmented(a : Array[T], lowerIncl : Int, upperExcl : Int)(implicit ordering : Ordering[T]): Array[T] = {
    var m = upperExcl - 1
    val base = lowerIncl
    buildHeap(a, lowerIncl, m, base)(ordering)
    while (m > lowerIncl) {
      swap(a, lowerIncl, m)
      m-=1
      heapify(a, lowerIncl, m, base)(ordering)
    }
    a
  }

  private def buildHeap(a: Array[T], lowerInсl : Int, m: Int, base : Int)(implicit ordering : Ordering[T]): Unit = {
    val mm = m - base
    for (i <- mm/2 to 0 by -1) {
      heapify(a, i + base, m, base)(ordering)
    }
  }

  private def swap(a: Array[T], i: Int, j:Int): Unit = {
    val staging = a(i)
    a(i) = a(j)
    a(j) = staging
  }
}
