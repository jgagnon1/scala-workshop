package training.scala.air_scala.aircraft

import training.scala.air_scala.airline.Passenger
import training.scala.air_scala.airport.{LongRunway, MediumRunway, ShortRunway, LandingSurface}

// To be used Later, maybe for abstract types
sealed trait AircraftManufacturer

case object Boeing extends AircraftManufacturer

case object McDonnellDouglas extends AircraftManufacturer

case object Airbus extends AircraftManufacturer

case object Bombardier extends AircraftManufacturer

sealed trait AircraftClass {
  val runwayType: LandingSurface
}

trait TurboProp extends AircraftClass {
  val runwayType = ShortRunway
}

trait NarrowBodyJet extends AircraftClass {
  val runwayType = MediumRunway
}

trait WideBodyJet extends AircraftClass {
  val runwayType = LongRunway
}

// todo: should we explain self type annotations?
trait AircraftModel {
  self: AircraftClass =>
  def seats: Map[SeatingClass, Seq[Seat]]

  def assignSeat(passenger: Passenger): Option[Seat]
}

class Plane(val economySeating: Seq[Seating[EconomySeat]],
            val economyPlusSeating: Seq[Seating[EconomyPlusSeat]] = Nil,
            val firstClassSeating: Seq[Seating[FirstClassSeat]] = Nil,
            val businessClassSeating: Seq[Seating[BusinessClassSeat]] = Nil) extends AircraftModel {
  self: AircraftClass =>

  def seats: Map[SeatingClass, Seq[Seat]] = seating.mapValues(_.map(_.seat))

  // Mutable map of seating assignment - Act as DB for now
  private var seating: Map[SeatingClass, Seq[Seating[Seat]]] = Map(
    BusinessClass -> businessClassSeating,
    FirstClass -> firstClassSeating,
    EconomyPlus -> economyPlusSeating,
    Economy -> economySeating
  )

  def assignSeat(passenger: Passenger): Option[Seat] = {
    val availableSeats: Iterable[Seat] = seating(passenger.seatingClass).collect {
      case seating if seating.isAvailable => seating.seat
    }

    // Try finding preference, fallback to Aisle, fallback to first available
    val selectedSeat = availableSeats.find(_.seatPosition == passenger.seatPreference) orElse
      availableSeats.find(_.seatPosition == Aisle) orElse
      availableSeats.headOption

    // Assign seat to the map
    selectedSeat.map { ss =>
      val seatings: Seq[Seating[Seat]] = seating(ss.seatingClass)
      // Update seating with the passenger
      val assignedSeat = Seating(ss, Some(passenger))
      seating = seating.updated(ss.seatingClass, assignedSeat +: seatings.filterNot(_.seat == ss))
      assignedSeat.seat
    }
  }


}

case class Seating[+C <: Seat](seat: C, passenger: Option[Passenger] = None) {
  def isAvailable: Boolean = passenger.isEmpty
}

case class Aircraft(model: AircraftModel)

case class Airline(name: String, aircraft: Set[Aircraft])

sealed trait SeatingClass {
  val priority: Int
}

trait FirstClassSeating extends SeatingClass {
  val priority = 1
}

case object FirstClass extends FirstClassSeating

trait BusinessClassSeating extends SeatingClass {
  val priority = 2
}

case object BusinessClass extends BusinessClassSeating

trait EconomyPlusSeating extends SeatingClass {
  val priority = 3
}

case object EconomyPlus extends EconomyPlusSeating

trait EconomySeating extends SeatingClass {
  val priority = 4
}

case object Economy extends EconomySeating

sealed trait SeatingPosition

sealed trait PreferedPosition

case object Aisle extends SeatingPosition with PreferedPosition

case object Middle extends SeatingPosition

case object Window extends SeatingPosition with PreferedPosition

sealed trait Seat {
  val row: Int
  val seat: Char
  val seatingClass: SeatingClass
  val seatPosition: SeatingPosition
}

// FIXME: Find a way to determine the seating position or instanciate it correctly
case class FirstClassSeat(row: Int, seat: Char, seatPosition: SeatingPosition = Aisle) extends Seat {
  final val seatingClass = FirstClass
}

case class BusinessClassSeat(row: Int, seat: Char, seatPosition: SeatingPosition = Aisle) extends Seat {
  final val seatingClass = BusinessClass
}

case class EconomyPlusSeat(row: Int, seat: Char, seatPosition: SeatingPosition = Aisle) extends Seat {
  final val seatingClass = EconomyPlus
}

case class EconomySeat(row: Int, seat: Char, seatPosition: SeatingPosition = Aisle) extends Seat {
  final val seatingClass = Economy
}
