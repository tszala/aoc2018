package com.tszala.aoc.day2

import com.tszala.aoc.utils.LinesReader

import scala.collection.immutable
import scala.util.{Failure, Success}

object Day2 {

  def main(args: Array[String]): Unit = {
    val filename = "src\\com\\tszala\\aoc\\day2\\input.txt"
    val inputs:List[String] = LinesReader.readTextFileWithTry(filename) match {
      case Success(l) => l
      case Failure(e) => println(e)
        System.exit(1)
        List.empty
    }

    val hash = inputs.map(s=>convertToCounts(s)).foldLeft((0,0))((a,b)=>(a._1 + b._1, a._2 + b._2))
    println(s"Computed value is ${hash._1 * hash._2}")


    val combinations: immutable.Seq[(String, String)] = for(a<-inputs; b<-inputs if (a != b)) yield (a,b)

    val result = combinations.filter(e => countDifferences(e._1,e._2)==1)
    println(s"Commont part is ${getCommonPart(result.head)}")
  }

  def countDifferences(str1: String, str2: String): Int = {
    var differences = 0;
    for(i <-0 to str1.length - 1)
      if(str1.charAt(i) != str2.charAt(i)) differences += 1

    differences
  }

  def getCommonPart(t: (String, String)): String = {
    var result = ""
    for(i <-0 to t._1.length - 1)
      if(t._1.charAt(i) == t._2.charAt(i)) result += t._1.charAt(i)

    result
  }


  def convertToCounts(line: String) : (Int, Int) = {
    val l = line.map(c=>(c,1))
      .groupBy(a=>a._1)
      .map(a=>(a._1, a._2.map(_._2).sum))
      .filter(a => a._2 == 2 || a._2 == 3).map(a=>a._2).toList

    (if(l.contains(2)) 1 else 0,if(l.contains(3)) 1 else 0)
  }

}
