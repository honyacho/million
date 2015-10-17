package models

import scala.slick.driver.MySQLDriver.simple._

case class User(id: Long, userId: String, userCompany: String, userDiscountRate: Int)

class Users(tag: Tag) extends Table[User](tag, "user") {
  def id               = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def userId           = column[String]("user_id")
  def userCompany      = column[String]("user_company")
  def userDiscountRate = column[Int]("user_discount_rate")

  def * = (id, userId, userCompany, userDiscountRate) <> ((User.apply _).tupled, User.unapply)
}
