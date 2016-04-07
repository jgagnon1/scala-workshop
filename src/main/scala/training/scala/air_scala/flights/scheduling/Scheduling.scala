package training.scala.air_scala.flights.scheduling

import com.github.nscala_time.time.Imports._
import squants.market.{USD, Money}
import training.scala.air_scala.airport.AirportCode
import training.scala.air_scala.flights.Flight

import scala.annotation.tailrec


object Itinerary {
  def layoverTimes(itinerary: ProposedItinerary): Seq[Duration] = itinerary.flights match {
    case _ :+ a :+ b =>
      itinerary.flights.sliding(2).map { case Seq(x, y, _*) => Duration.millis(
      y.schedule.origin.time.getMillis -
      x.schedule.destination.time.getMillis)}.toSeq
    case _ => Seq.empty[Duration]
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
