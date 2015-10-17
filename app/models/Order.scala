package models

import scala.slick.driver.MySQLDriver.simple._

case class Order(
  id: Long,
  orderId: String,
  orderDateTime: Long,
  orderUserId: String,
  orderItemId: String,
  orderQuantity: Int,
  orderState: String,
  orderTags: String
)

class Orders(tag: Tag) extends Table[Order](tag, "order") {
  def id              = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def orderId         = column[String]("order_id")
  def orderDateTime   = column[Long]("order_date_time")
  def orderUserId     = column[String]("order_user_id")
  def orderItemId     = column[String]("order_item_id")
  def orderQuantity   = column[Int]("order_quantity")
  def orderState      = column[String]("order_state")
  def orderTags       = column[String]("order_tags")

  def * = (
    id,
    orderId,
    orderDateTime,
    orderUserId,
    orderItemId,
    orderQuantity,
    orderState,
    orderTags
  ) <> ((Order.apply _).tupled, Order.unapply)
}
