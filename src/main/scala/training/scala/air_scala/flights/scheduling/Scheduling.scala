package training.scala.air_scala.flights.scheduling

import com.github.nscala_time.time.Imports._
import training.scala.air_scala.aircraft.Seat
import training.scala.air_scala.airline.Passenger
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
    (this.flights, that.flights) match {
      case ((h +: _), (thatH +: _)) => h compare thatH
      case ((h +: _), _) => 1 // `that` is empty
      case (_, (thatH +: _)) => -1 // `this` is empty
      case (Nil, Nil) => 0 // Both empty
    }
  }

  def checkInPassenger(passenger: Passenger): Seq[(Flight, Seat)] = {
    flights.map {flight =>
      val seat = Flight.checkinPassenger(passenger, flight)
      (flight, seat)
    }
  }
}

case class ProposedItinerary(flights: Seq[Flight]) extends Itinerary
