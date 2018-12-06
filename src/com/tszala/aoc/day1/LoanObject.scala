package com.tszala.aoc.day1

object LoanObject {
    def using[A <: {def close(): Unit}, B](resource: A)(f: A=>B): B =
      try {
        f(resource)
      } finally {
        resource.close()
      }
}
