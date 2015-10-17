package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.libs.json._

import scala.slick.driver.MySQLDriver.simple._

object Application extends Controller {

  val user = TableQuery[Users]
  val order = TableQuery[Orders]

  def getOrder(orderId: String) = Action {
    Logger.info(orderId)
    globals.db.withSession{implicit s =>
      order.where(_.orderId === orderId).firstOption.fold(
        {
          val res = NotFound(Json.obj(
            "result" -> false,
            "data"   -> JsNull
          ))
          res
        }
      )(o =>
        Ok(Json.obj(
          "result" -> true,
          "data"   -> Json.obj(
            "orderId"       -> o.orderId,
            "orderDateTime" -> o.orderDateTime,
            "orderUserId"   -> o.orderUserId,
            "orderItemId"   -> o.orderItemId,
            "orderQuantity" -> o.orderQuantity,
            "orderState"    -> o.orderState,
            "tags"          -> o.orderTags.split(",")
          )
        ))
      )
    }
  }

  private def ifemptyTrueOp(cond: Column[Option[Boolean]], opt: Option[_]) =
    if(opt.isEmpty) LiteralColumn(Option(true)) else cond

  def searchOrder(
    timeGte: Option[Long],
    timeLte: Option[Long],
    userId: Option[String],
    itemId: Option[String],
    quantityGte: Option[Int],
    quantityLte: Option[Int],
    orderState: Option[String],
    tagsAll: Option[String],
    tagsAny: Option[String],

    userCompany: Option[String],
    discountRateGte: Option[Int],
    discountRateLte: Option[Int],

    itemSupplier: Option[String],
    itemStockQuantityGte: Option[Int],
    itemStockQuantityLte: Option[Int],
    itemBasePriceGte: Option[Int],
    itemBasePriceLte: Option[Int],
    itemTagsAll: Option[String],
    itemTagsAny: Option[String]
  ) = Action {
    globals.db.withSession{ implicit s =>
      val q1 = order.where{ row =>
        ifemptyTrueOp(row.orderDateTime >= timeGte, timeGte)         &&
        ifemptyTrueOp(row.orderDateTime <= timeLte, timeLte)         &&
        ifemptyTrueOp(row.orderUserId === userId, userId)            &&
        ifemptyTrueOp(row.orderItemId === itemId, itemId)            &&
        ifemptyTrueOp(row.orderQuantity >= quantityGte, quantityGte) &&
        ifemptyTrueOp(row.orderQuantity >= quantityLte, quantityLte) &&
        ifemptyTrueOp(row.orderState === orderState, orderState)
      }.sortBy(_.orderDateTime.desc)

      val q2 = user.where { row =>
        ifemptyTrueOp(row.userCompany      === userCompany, userCompany)        &&
        ifemptyTrueOp(row.userDiscountRate >= discountRateGte, discountRateGte) &&
        ifemptyTrueOp(row.userDiscountRate <= discountRateLte, discountRateLte)
      }

      val li = q1.take(100).list.map(o =>
        Json.obj(
          "orderId"       -> o.orderId,
          "orderDateTime" -> o.orderDateTime,
          "orderUserId"   -> o.orderUserId,
          "orderItemId"   -> o.orderItemId,
          "orderQuantity" -> o.orderQuantity,
          "orderState"    -> o.orderState,
          "tags"          -> o.orderTags.split(",")
        )
      )
      Ok(Json.obj(
        "result" -> true,
        "data"   -> li
      ))
    }
  }
}
