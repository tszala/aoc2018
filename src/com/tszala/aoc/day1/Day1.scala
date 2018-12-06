package com.tszala.aoc.day1

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day1 {
  def main(args: Array[String]) = {
    val filename = "input.txt"
    val frequencies:List[Int] = readTextFileWithTry(filename) match {
      case Success(l) => l.map(_.toInt)
      case Failure(e) => println(e)
        List.empty
    }

    val newFrequency = frequencies.foldLeft(0)((a,b) => a+b)
    println(s"New frequency is $newFrequency")
  }

  def readTextFileWithTry(f:String) :Try[List[String]] = {
    Try {
      val lines = LoanObject.using(Source.fromFile(f)) { source =>
        (for (line <- source.getLines) yield line).toList
      }
      lines
    }
  }


}
