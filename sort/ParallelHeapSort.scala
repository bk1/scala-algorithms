import reflect.ClassTag

/**
 * Implements an example of Merge sort.
 *
 * In English, here's how the algorithm works:
 * Recursively split the array in half, sort each subarray, and then merge
 * the sorted arrays into a single, sorted array, by traversing both arrays. 
 * The base case occurs when the subarray is a single element (considered 
 * sorted).
 */

object ParallelHeapSort {
  def main(args: Array[String]) {
    val phs : ParallelHeapSort[Int] = new ParallelHeapSort(2)
    var mess = Array(3, 9, 8, 13, 2, 5, 4);
    mess.foreach( println )
    println
    val result = phs.sort(mess)
    result.foreach( println )
  }
}

class ParallelHeapSort[T : Ordering](val nThreads : Int) {

  /** Recursively sorts a subarray via Merge Sort algorithm.
   *
   * @param a an unsorted array to be sorted 
   * @param lowerIncl the index of the left-most edge of desired subarray
   * @param higherExcl the index of the right-most edge of desired subarray
   * @return a sorted array containing all elements in the prescribed subarray
   */
  def sort(a: Array[T])(implicit ordering : Ordering[T], classTag : ClassTag[T]) : Array[T] = {
    sortSegmented(a, 0, a.length)(ordering, classTag)
  }

  def sortSegmented(a: Array[T], lowerIncl: Int, higherExcl: Int)(implicit ordering : Ordering[T], classTag : ClassTag[T]) : Array[T] = {
    println("sortSegmented lowerIncl=" + lowerIncl + " higherExcl=" + higherExcl)
    if (higherExcl - lowerIncl <= 1){
      println("slicing")
      return a.slice(lowerIncl, higherExcl)
    }
    val mid = (higherExcl - lowerIncl)/2 + lowerIncl
    println("sortSegmented lowerIncl=" + lowerIncl + " higherExcl=" + higherExcl + " mid=" + mid)
    val left = sortSegmented(a, lowerIncl, mid)(ordering, classTag)
    val right = sortSegmented(a, mid, higherExcl)(ordering, classTag)

    println("merging")
    return merge(left, 0, left.length, right, 0, right.length)(ordering, classTag)
  }

  /** Combines two sorted arrays into a single sorted array. 
   *
   * @param a first sorted array
   * @param b second sorted array
   * @return a new sorted array containing all elements from a and b
   *  */
  def merge(a: Array[T], loA : Int, hiA : Int, b: Array[T], loB : Int, hiB : Int)(implicit ordering : Ordering[T], classTag : ClassTag[T]): Array[T] = {
    println("merge : " + a.length + " " + b.length)
    var result = Array.fill(hiA - loA + hiB - loB)(a(0))
    val imax = hiA - loA
    val jmax = hiB - loB
    var i: Int = 0
    var j: Int = 0

    /** Note: this could also be done with recursion,
     *  which would probably be more idiomatic of 
     *  functional programming.
     */
    while (i < imax || j < jmax){
      if (i >= imax) {
        result(i+j) = b(j)
        j+=1
      } else if (j >= jmax || ordering.compare(a(i), b(j)) <= 0) {
        result(i+j) = a(i)
        i+=1
      } else {
        result(i+j) = b(j)
        j+=1
      }
    }
    println("merge : " + a.length + " " + b.length + " -> " + result.length)
    return result
  }
}
