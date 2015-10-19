/**
 * Implements an example of Bucket Sort.
 *
 * In English, here's how the algorithm works:
 * Iterate over the input, segregating elements into a fixed number of ordinal
 * buckets. The sort each bucket. Iterate over the buckets in order,
 * reconstructing the original input in sorted order.
 **/
import scala.collection.mutable.ArrayBuffer

object BucketSort {
  def main(args: Array[String]) {
    var mess = Array(3, 9, 8, 13, 2, 5, 4, 12);

    // Move contents to an ArrayBuffer since
    // our recursive sort requires a mutable
    // data structure. This could be avoided
    // in a hybrid bucketsort/insertionsort
    val ab = new ArrayBuffer[Int]
    ab ++= mess
    sort(ab, 5, 2, 13)

    ab.foreach( println )
  }

  def sort(a: ArrayBuffer[Int], n_buckets: Int, min: Int, max: Int) : Unit = {
    if (a.length < 2) return //a bucket could be empty, or have one element

    val buckets = ArrayBuffer.fill(n_buckets,0)(0)
    for (i <- 0 until n_buckets) {
      buckets(i) = new ArrayBuffer[Int]
    }

    for (value <- a) {
      val bucket_no = math.min((value - min) * n_buckets / (max - min), n_buckets - 1)
      buckets(bucket_no) += value
    }

    // we'll overwrite 'a' to save space
    val width = (max - min) / n_buckets
    var i = 0
    var b = 0
    for (bucket <- buckets) {
      sort(bucket, n_buckets, 
           min + b * (max - min) / n_buckets, 
           min + (b + 1) * (max - min) / n_buckets )
      for (value <- bucket) {
        a(i) = value
        i+=1
      }
      b+=1
    }
  }

}
