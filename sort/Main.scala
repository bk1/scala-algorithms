/** -*- coding: utf-8-unix -*- Юникод/UTF-8
 *
 * Main program to test sorting algorithms
 */

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Vector
import scala.collection.immutable.Seq

object Main {
  def usage() : Unit = {
    println("Usage:\nscala Main inputFile outputFile algorithm\n")
    println("Algorithms: heapsort native heapsort-array\n")
    System.exit(0)
  }

  def output(list : Iterable[String], outputFile : String) : Unit = {
    // FileWriter
    val file = new File(outputFile)
    val bw   = new BufferedWriter(new FileWriter(file))
    list.foreach((x : String) => { bw.write(x+"\n")})
    bw.close()
  }

  def main(args : Array[String]) : Unit = {
    if (args.length != 3) {
      usage()
    }
    val inputFile : String = args(0)
    val outputFile : String = args(1)
    val algorithm : String = args(2)
    println("inputFile=" + inputFile);
    println("outputFile=" + outputFile);
    println("algorithm=" + algorithm);
    
    val source = Source.fromFile(new File(inputFile), "utf-8")
    val linesInternal = source.getLines
    // println("linesInternal=" + linesInternal)
    val listOfLines = linesInternal.toList
    if (algorithm == "native") {
      val unsorted = listOfLines.to[Seq]
      val sorted = unsorted.sorted
      val sortedList = sorted.to[Seq]
      output(sortedList, outputFile)
    } else if (algorithm == "heapsort") {
      val unsorted = listOfLines.to[ArrayBuffer]
      val hs : HeapSortList[String] = new HeapSortList()
      val sorted = hs.sort(unsorted)
      val sortedList = sorted.to[Seq]
      output(sortedList, outputFile)
    } else if (algorithm == "heapsort-array") {
      val unsorted = listOfLines.to[Array]
      val hs : HeapSortArray[String] = new HeapSortArray()
      val sorted = hs.sort(unsorted)
      val sortedList = sorted.to[Seq]
      output(sortedList, outputFile)
    } else if (algorithm == "heapsort-seq") {
      val unsorted = listOfLines.to[Seq]
      val hs : HeapSortSeq[String] = new HeapSortSeq()
      val sorted = hs.sort(unsorted)
      val sortedList = sorted.to[Seq]
      output(sortedList, outputFile)
    } else {
      usage()
    }
  }
}
