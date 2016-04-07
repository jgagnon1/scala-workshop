package training.scala.air_scala.airline

import training.scala.air_scala.aircraft._

/**
 * Perfect core for future exercise regarding Abstract Types...
 * Passenger would have an abstract type Nationality, which would
 * need to be filled in on a concrete instance of Passenger
 *
 * Also, add "FrequentFlyer" info... maybe instead?
 */
sealed trait Passenger {
  type sClass <: SeatingClass

  def familyName: String

  def givenName: String

  def middleName: Option[String]

  def seatPreference: PreferedPosition

  def seatingClass: sClass
}

//case class Canadian(familyName: String, givenName: String, middleName: Option[String], seatPreference: PreferedPosition) extends Passenger
//case class American(familyName: String, givenName: String, middleName: Option[String], seatPreference: PreferedPosition) extends Passenger

case class FirstClassPassenger(familyName: String, givenName: String, middleName: Option[String], seatPreference: PreferedPosition) extends Passenger {
  type sClass = FirstClassSeating

  def seatingClass = FirstClass
}

case class BusinessClassPassenger(familyName: String, givenName: String, middleName: Option[String], seatPreference: PreferedPosition) extends Passenger {
  type sClass = BusinessClassSeating

  def seatingClass = BusinessClass
}
