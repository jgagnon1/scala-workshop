package training.scala.air_scala.flights.scheduling

import com.github.nscala_time.time.Imports._
import training.scala.air_scala.flights.Flight


object Itinerary {
  def layoverTimes(itinerary: ProposedItinerary): Seq[Duration] = {
    itinerary.flights.sliding(2).flatMap {
      case Seq(x, y) =>
        val d = Duration.millis(
          y.schedule.origin.time.getMillis -
            x.schedule.destination.time.getMillis)
        Some(d)
      case _ => None
    }.toSeq
  }
}

sealed trait Itinerary extends Ordered[Itinerary] {
  val flights: Seq[Flight]

  override def compare(that: Itinerary): Int = {
    this.flights match {
      case h +: _ =>
        that.flights match {
          case thatH +: _ => h compare thatH
          case _ => 1
        }
      case _ if that.flights.isEmpty => 0
      case _ => -1
    }
  }
}

case class ProposedItinerary(flights: Seq[Flight]) extends Itinerary
