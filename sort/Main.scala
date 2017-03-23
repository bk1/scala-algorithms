/** -*- coding: utf-8-unix -*- Юникод/UTF-8
 *
 * Main program to test sorting algorithms
 */

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {
  def usage() : Unit = {
    println("Usage:\nscala Main inputFile outputFile algorithm\n")
    println("Algorithms: heapsort\n")
    System.exit(0)
  }

  def output(list : List[String], outputFile : String) : Unit = {
    // FileWriter
    val file = new File(outputFile)
    val bw   = new BufferedWriter(new FileWriter(file))
    list.foreach { bw.write(_) }
    bw.close()
  }

  def main(args : Array[String]) : Unit = {
    if (args.length != 3) {
      usage()
    }
    val inputFile : String = args(0)
    val outputFile : String = args(1)
    val algorithm : String = args(2)
    val listOfLines = Source.fromFile(inputFile).getLines.toList
    if (algorithm == "heapsort") {
      val unsorted = listOfLines.to[ArrayBuffer]
      val hs : HeapSortList[String] = new HeapSortList()
      val sorted = hs.sort(unsorted)
      val sortedList = sorted.to[List]
      output(sortedList, outputFile)
    } else {
      usage()
    }
  }
}
