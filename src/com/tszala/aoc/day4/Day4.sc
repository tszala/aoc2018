import java.security
import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

import com.sun.corba.se.impl.orbutil.fsm.GuardedAction
import com.sun.xml.internal.ws.message.source.ProtocolSourceMessage
import com.tszala.aoc.utils.LinesReader

val inputs = LinesReader.readLinesOrExit("d:\\AoC2018\\aoc2018\\src\\com\\tszala\\aoc\\day4\\input.txt")

val inputs2 = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up".split("\n")

val pattern = "[(\\.+)]"r
val guardNumberPattern = "Guard #([0-9]+) begins shift"r

case class Guard(number: Int)

sealed trait Action
case class GuardAction(guard:Guard, time: java.time.LocalDateTime, action: Action)
case class BeginsShift() extends Action
case class FallsAsleep() extends Action
case class WakesUp() extends Action
case class UnknownAction() extends Action

val sortedShifts = inputs.map(input => {
  val date = input.substring(1,input.indexOf(']'))
  val text = input.substring(input.indexOf(']') + 2)
  (java.time.LocalDateTime.parse(date,DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")),text)
}).sortWith((a,b) => b._1.isAfter(a._1))

var currentGuard:Option[Guard] = None
val guardActions: Seq[GuardAction] = for(shift <- sortedShifts) yield {
  val shiftDescription = shift._2
  if(shiftDescription.indexOf('#') > 0) {
    val guardNumberPattern(guardNumber) = shiftDescription
    currentGuard = Some(Guard(guardNumber.toInt))
    GuardAction(currentGuard.get,shift._1,BeginsShift())
  } else {
    shiftDescription match {
      case "wakes up" => GuardAction(currentGuard.get, shift._1,WakesUp())
      case "falls asleep" => GuardAction(currentGuard.get, shift._1, FallsAsleep())
      case _ => GuardAction(currentGuard.get, shift._1, UnknownAction())
    }
  }
}

def localDateTimeToSeconds(date:LocalDateTime) = date.atZone(ZoneOffset.UTC).toEpochSecond

def getMinutesBetweenDates(d1:LocalDateTime, d2:LocalDateTime) = {
  val m = localDateTimeToSeconds(d2) / 60 - localDateTimeToSeconds(d1) / 60

  def countInternal(start: Int, count: Long, minutes:List[Int]):List[Int] = {
    count match {
      case 0 => minutes
      case i => countInternal(if((start + 1) % 60 == 0) 1 else start + 1, count - 1, start::minutes)
    }
  }
  countInternal(d1.getMinute, m,List.empty)
}

def countSleepTime(actions: List[GuardAction]):(Long, List[Int]) = {
  def countSleepTimeInternal(actions:List[GuardAction], sleepTime: java.time.LocalDateTime, acc:(Long, List[Int])): (Long, List[Int]) = {
    actions match {
      case Nil => acc
      case head::tail => head.action match {
        case FallsAsleep() => countSleepTimeInternal(tail,head.time,acc)
        case WakesUp() => countSleepTimeInternal(tail,head.time,(acc._1+localDateTimeToSeconds(head.time) - localDateTimeToSeconds(sleepTime), acc._2:::getMinutesBetweenDates(sleepTime,head.time)))
        case _ => countSleepTimeInternal(tail,sleepTime,acc)
      }
    }
  }
  countSleepTimeInternal(actions,actions.head.time,(0L, List.empty))
}

val guardSleeps: Map[Guard, (Long, List[Int])] = guardActions
  .groupBy(g=>g.guard)
  .mapValues(actions => actions.sortWith((a,b)=>b.time.isAfter(a.time)))
  .mapValues(actions => countSleepTime(actions.toList))

val longestSleepingGuard = guardSleeps.maxBy(a=>a._2._1)
//minute and frequency
val (minute, frequency) = longestSleepingGuard._2._2.groupBy(x=>x).mapValues(elems=>elems.size).maxBy(elem=>elem._2)
println(s"The minute in which the guard ${longestSleepingGuard._1.number} is sleeping is ${minute}")

val groupedMinutesByGuards: Map[Guard, (Int, Int)] =
  guardSleeps.mapValues(v => v._2.groupBy(x=>x).mapValues(x=>x.size).toList)
    //take guard with some minutes
    .filter(guard => guard._2.size > 0)
  .mapValues(_.maxBy(e=>e._2))

val part2Guard = groupedMinutesByGuards.maxBy(g=>g._2._2)
println(s"The most sleeping guard is ${part2Guard._1.number} with minute ${part2Guard._2._1} sleeping ${part2Guard._2._2}, result is ${part2Guard._1.number * part2Guard._2._1}")





