package models

import scala.slick.driver.MySQLDriver.simple._

case class Item(
  id: Long,
  itemId: String,
  itemSupplier: String,
  itemStockQuantity: Int,
  itemBasePrice: Int,
  itemTags: String
)

class Items(tag: Tag) extends Table[Item](tag, "item") {
  def id                = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def itemId            = column[String]("item_id")
  def itemSupplier      = column[String]("item_supplier")
  def itemStockQuantity = column[Int]("item_stock_quantity")
  def itemBasePrice     = column[Int]("item_base_price")
  def itemTags          = column[String]("item_tags")

  def * = (
    id,
    itemId,
    itemSupplier,
    itemStockQuantity,
    itemBasePrice,
    itemTags
  ) <> ((Item.apply _).tupled, Item.unapply)
}
