package com.tszala.aoc.utils

import scala.io.Source
import scala.util.Try

object LinesReader {

  def readTextFileWithTry(f:String) :Try[List[String]] = {
    Try {
      val lines = LoanObject.using(Source.fromFile(f)) { source =>
        (for (line <- source.getLines) yield line).toList
      }
      lines
    }
  }

}
