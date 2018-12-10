package com.tszala.aoc.day3

import com.tszala.aoc.utils.LinesReader

import scala.util.{Failure, Success}

object Day3 {

  case class ElvenPlan(number: Int, xOffset:Int, yOffset:Int, x:Int, y:Int)

  val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r


  def main(args: Array[String]): Unit = {
    val filename = "src\\com\\tszala\\aoc\\day3\\input.txt"
    val inputs: List[String] = LinesReader.readLinesOrExit(filename)

    //val inputs2 = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2".split('\n').toList

    val plans = inputs.map(input => {
      val pattern(number, xOffset,yOffset,from,to) = input
      ElvenPlan(number.toInt,xOffset.toInt,yOffset.toInt,from.toInt,to.toInt)
    })

    val coordinates = getCoordinates(plans)
    val groupedD = coordinates.groupBy(x=>x).filter(el => el._2.size > 1).keySet
    println(s"Number of doubled Elven plans ${groupedD.size}")


    val coordinatesWithNumbers = plans.flatMap(planToCoordinatesWithNumber(_))
    val groupedByCoordinates: Map[(Int, Int), List[(ElvenPlan, (Int, Int))]] = coordinatesWithNumbers.groupBy(x=>x._2)
    val single = groupedByCoordinates.filter(el => el._2.size >  1).flatMap(_._2).map(_._1).toSet

    println(s"Not overlapping claim is ${plans.filter(plan => !single.contains(plan))}")

  }

  private def getCoordinates(plans: List[ElvenPlan]) = {
    plans.flatMap(planToCoordinates(_))
  }

  def planToCoordinates(ep:ElvenPlan):List[(Int,Int)] =
    (for(i <- ep.xOffset to (ep.xOffset + ep.x - 1);
         j <- ep.yOffset to (ep.yOffset + ep.y - 1)) yield (i,j)).toList

  def planToCoordinatesWithNumber(ep:ElvenPlan):List[(ElvenPlan,(Int,Int))] =
    (for(i <- ep.xOffset to (ep.xOffset + ep.x - 1);
         j <- ep.yOffset to (ep.yOffset + ep.y - 1)) yield (ep,(i,j))).toList

  def planToSquareInches(elvenPlan: ElvenPlan) = elvenPlan.x * elvenPlan.y
}
