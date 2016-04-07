package training.scala.air_scala.flights

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import squants.market.Money
import squants.space._
import training.scala.air_scala.aircraft.Aircraft
import training.scala.air_scala.airport.AirportCode
import training.scala.air_scala.flights.scheduling.{ProposedItinerary, Itinerary}

case class Flight(val number: FlightNumber,
  val aircraft: Aircraft,
  val schedule: Schedule,
  val price: Money,
  val miles: Length) extends Ordered[Flight] {

  override val toString = s"Flight { number: $number, aircraft: $aircraft, schedule: $schedule, " +
    s"price: $price, miles: $miles }"

  override def compare(that: Flight): Int = this.schedule.compare(that.schedule)

  def flightDuration: Period = schedule.duration

  /**
   * This may seem odd at first, but subtracting two flights needs to give us the COMBINED duration
   * of those flights, as such we *ADD* their durations.
   *
   */
  def -(that: Flight): Period = new Period(that.schedule.origin.time, this.schedule.destination.time)

}

case class FlightNumber(airlineCode: String, flightNumber: Int) {
  require(airlineCode.matches("[A-Z]{1,3}"),
    "Airline Code must consist of 1-3 uppercase letters.")
  require(flightNumber > 0 && flightNumber < 8999,
    "Flight Number be between 1 & 8999")
}

case class FlightLeg(code: AirportCode, time: DateTime) extends Ordered[FlightLeg] {
  override def compare(that: FlightLeg): Int = this.time.compareTo(that.time)
}

case class Schedule(origin: FlightLeg, destination: FlightLeg) extends Ordered[Schedule] {
  override def compare(that: Schedule): Int =
    this.origin.compare(that.origin) + this.destination.compare(that.destination)

  /**
   * A time period representing the amount (Duration) of time between the origin and destination
   */
  def duration: Period = new Period(origin.time, destination.time)

  /**
   * This may seem odd at first, but subtracting two schedules needs to give us the COMBINED duration
   * of those schedules, as such we *ADD* their durations.
   *
   * NOTE:  normalizedStandard() on Period prevents oddities like "19 hours, 70 minutes"
   */
  def -(that: Schedule): Period = (this.duration + that.duration).normalizedStandard()
}

class FlightPlanner(availableFlights: Set[Flight]) {

  def proposeItineraries(departAfter: DateTime)
                        (from: AirportCode, to: AirportCode)
                        (minConnectionTime: Duration = Period.minutes(90).toStandardDuration)
                        (maxConnections: Int = 2): Set[ProposedItinerary] = {
    require(from != to, "Departing and arriving airport must be different.")

    def propose(from: AirportCode, to: AirportCode, departAfter: DateTime): Set[Seq[Flight]] = {
      if (from == to)
        Set(Seq.empty[Flight])
      else for {
        flight <- availableFlights
        if flight.schedule.origin.code == from && flight.schedule.origin.time > departAfter
        dest = flight.schedule.destination.code
        pathFromDest <- propose(dest, to, flight.schedule.destination.time)
      } yield flight +: pathFromDest
    }

    // TODO : Filter minConnection and maxConnections
    val proposed: Set[ProposedItinerary] = propose(from, to, departAfter)
      .map (ProposedItinerary)


    val withMaxConn = FlightPlanner.filterByMaxConnections(maxConnections)(proposed)
    val withMinConnTime = FlightPlanner.filterByMinConnectionTime(minConnectionTime)(proposed)

    withMaxConn intersect withMinConnTime
  }


}

object FlightPlanner {

  def filterByMaxConnections(maxConnections: Int)(itineraries: Set[ProposedItinerary]): Set[ProposedItinerary] = {
    itineraries.filter { it =>
      val itConnections = it.flights.size - 1
      itConnections <= maxConnections
    }
  }

  def filterByMinConnectionTime(minConnectionTime: Duration)(itineraries: Set[ProposedItinerary]): Set[ProposedItinerary] =
    itineraries.filter { it =>
    val layovers: Seq[_root_.com.github.nscala_time.time.Imports.Duration] = Itinerary.layoverTimes(it)
    !layovers.exists(_ <= minConnectionTime)
  }
}