package com.tszala.aoc.day5

import com.tszala.aoc.utils.LinesReader

object Day5 {

  def main(args: Array[String]): Unit = {
    val input = LinesReader.readLinesOrExit("src/com/tszala/aoc/day5/input.txt").head

    //val input2 = "dabAcCaCBAcCcaDA"

    val reduced = reducePolymers(input)
    println(s"Size of reduced unit of polymers is ${reduced.length}")
  }

  def reducePolymers(input:String) : String = {
    def reducePolymersInternal(input: String, reduced: String):String = if(input.length == reduced.length) reduced else reducePolymersInternal(reduced, reduceOnce(reduced))
    reducePolymersInternal(input, reduceOnce(input))
  }

  def reduceOnce(input:String) = {

    def reduceOnceInternal(in:Seq[Char], acc: String):String =
      in match {
        case Nil => acc
        case c::Nil => acc+c
        case head::tail => reduceOnceInternal(if(opposites(head,tail.head)) tail.tail else tail,  if (opposites(head,tail.head)) acc else acc + head)
        case _ => throw new Exception("Unknown sequence")
      }

    reduceOnceInternal(input.toList,"")
  }

  def opposites(c1:Char, c2:Char) = Math.abs(c2 - c1) == 32

}
