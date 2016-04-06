package training.scala.air_scala.airline

import training.scala.air_scala.aircraft.{PreferedPosition, SeatingPosition, SeatingClass}

/**
 * Perfect core for future exercise regarding Abstract Types...
 * Passenger would have an abstract type Nationality, which would
 * need to be filled in on a concrete instance of Passenger
 *
 * Also, add "FrequentFlyer" info... maybe instead?
 */
sealed trait Passenger {
  type seatingClass <: SeatingClass

  def familyName: String

  def givenName: String

  def middleName: Option[String]

  def seatPreference: PreferedPosition
}

case class Canadian(familyName: String, givenName: String, middleName: Option[String], seatPreference: PreferedPosition) extends Passenger
case class American(familyName: String, givenName: String, middleName: Option[String], seatPreference: PreferedPosition) extends Passenger


