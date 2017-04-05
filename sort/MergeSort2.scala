/**
 * Implements an example of Merge sort.
 *
 * In English, here's how the algorithm works:
 * Recursively split the array in half, sort each subarray, and then merge
 * the sorted arrays into a single, sorted array, by traversing both arrays. 
 * The base case occurs when the subarray is a single element (considered 
 * sorted).
 */

object MergeSort2 {
  def main(args: Array[String]) {
    val phs : MergeSort2[Int] = new MergeSort2(2)
    var mess = Array(3, 9, 8, 13, 2, 5, 4);
    mess.foreach( println )
    println
    val result = phs.sort(mess)
    result.foreach( println )
  }
}

class MergeSort2[T : Ordering](val nThreads : Int) {

  /** Recursively sorts a subarray via Merge Sort algorithm.
   *
   * @param a an unsorted array to be sorted 
   * @param lowerIncl the index of the left-most edge of desired subarray
   * @param higherExcl the index of the right-most edge of desired subarray
   * @return a sorted array containing all elements in the prescribed subarray
   */
  def sort(a: Array[Int]) : Array[Int] = {
    sortSegmented(a, 0, a.length)
  }

  def sortSegmented(a: Array[Int], lowerIncl: Int, higherExcl: Int) : Array[Int] = {
    println("sortSegmented lowerIncl=" + lowerIncl + " higherExcl=" + higherExcl)
    if (higherExcl - lowerIncl <= 1){
      println("slicing")
      return a.slice(lowerIncl, higherExcl)
    }
    val mid = (higherExcl - lowerIncl)/2 + lowerIncl
    println("sortSegmented lowerIncl=" + lowerIncl + " higherExcl=" + higherExcl + " mid=" + mid)
    val left = sortSegmented(a, lowerIncl, mid)
    val right = sortSegmented(a, mid, higherExcl)

    println("merging")
    return merge(left, right)
  }

  /** Combines two sorted arrays into a single sorted array. 
   *
   * @param a first sorted array
   * @param b second sorted array
   * @return a new sorted array containing all elements from a and b
   *  */
  def merge(a: Array[Int], b: Array[Int]): Array[Int] = {
    println("merge : " + a.length + " " + b.length)
    var result = Array.fill(a.length + b.length)(0)

    var i: Int = 0
    var j: Int = 0

    /** Note: this could also be done with recursion,
     *  which would probably be more idiomatic of 
     *  functional programming.
     */
    while (i < a.length || j < b.length){
      if (i >= a.length){
        result(i+j) = b(j)
        j+=1
      }
      else if (j >= b.length || a(i) <= b(j)) {
        result(i+j) = a(i)
        i+=1
      }
      else {
        result(i+j) = b(j)
        j+=1
      }
    }
    println("merge : " + a.length + " " + b.length + " -> " + result.length)
    return result
  }
}
