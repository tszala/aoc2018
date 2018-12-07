package com.tszala.aoc.day1

import com.tszala.aoc.utils.{LinesReader}

import scala.util.{Failure, Success}

object Day1 {
  def main(args: Array[String]) = {
    val filename = "src\\com\\tszala\\aoc\\day1\\input.txt"
    val frequencies:List[Int] = LinesReader.readTextFileWithTry(filename) match {
      case Success(l) => l.map(_.toInt)
      case Failure(e) => println(e)
        System.exit(1)
        List.empty
    }

    val newFrequency = frequencies.sum
    println(s"New frequency is $newFrequency")

    val foundFrequencies: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set[Int]()
    foundFrequencies.add(0)

    //println(s"Duplicate frequency is ${findDuplicateFrequency(frequencies,0,foundFrequencies)}")

    println(s"Duplicate frequency is ${findDuplicateFrequencyLoops(frequencies,0)}")
  }

  def findDuplicateFrequency(inputs:List[Int], currentValue: Int, frequencies: scala.collection.mutable.Set[Int]): Int = {
    inputs match {
      case Nil => findDuplicateFrequency(inputs,currentValue,frequencies)
      case head::tail => {
        val newValue = currentValue + head
        if(frequencies.contains(newValue)) return newValue else {
          frequencies.add(newValue)
          findDuplicateFrequency(tail, newValue, frequencies)
        }
      }
    }
  }


  def findDuplicateFrequencyLoops(inputs:List[Int], currentValue: Int): Int = {
    var found = false
    var acc = currentValue
    val foundFrequencies: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set[Int]()
    foundFrequencies.add(acc)

    while(!found) {
      val elements = inputs
      for(a <- elements if !found) {
        acc += a

        if(foundFrequencies.contains(acc)) found = true else foundFrequencies.add(acc)

      }
    }
    acc
  }



}
