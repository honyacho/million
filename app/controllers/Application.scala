package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.libs.json._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import Q.interpolation
import scala.annotation.tailrec

import scala.slick.driver.MySQLDriver.simple._

object Application extends Controller {

  val user = TableQuery[Users]
  val order = TableQuery[Orders]

  def getOrder(orderId: String) = Action {
    Logger.info(orderId)
    globals.db.withSession{implicit s =>
      order.filter(_.orderId === orderId).firstOption.fold(
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
    itemTagsAny: Option[String],

    limit: Option[Int]
  ) = Action {
    globals.db.withSession{ implicit s =>
      val q1 = query1(timeGte, timeLte, userId, itemId, quantityGte, quantityLte, orderState, tagsAll, tagsAny, limit)
      val res = Q.queryNA[String](q1).list
      Ok(Json.obj(
        "result" -> true,
        "data"   -> Json.parse(res.mkString("[", ",", "]"))
      ))
    }
  }

  def query1(
    timeGte: Option[Long],
    timeLte: Option[Long],
    userId: Option[String],
    itemId: Option[String],
    quantityGte: Option[Int],
    quantityLte: Option[Int],
    orderState: Option[String],
    tagsAll: Option[String],
    tagsAny: Option[String],
    limit: Option[Int]
  ): String = {
    val sb = new StringBuilder()
    val conds = new scala.collection.mutable.ArrayBuffer[String]()
    timeGte.map("o.order_date_time >= " + _).foreach(conds += _)
    timeLte.map("o.order_date_time <= " + _).foreach(conds += _)
    userId.map(i => s"o.order_user_id = '${i}'").foreach(conds += _)
    itemId.map(i => s"o.order_item_id = '${i}'").foreach(conds += _)
    quantityGte.map("o.order_quantity >=" + _).foreach(conds += _)
    quantityLte.map("o.order_quantity <=" + _).foreach(conds += _)
    orderState.map(i => "o.order_state = '${i}'").foreach(conds += _)
    sb.append("select o.json from `order` o")
    if (conds.nonEmpty) {
      sb.append(" where ")
      sb.append(conds mkString " and ")
    }
    sb.append(" limit ")
    sb.append(limit.getOrElse(100).toString)
    // tagsAll
    // tagsAny
    sb.result
  }

  @tailrec
  def bulkInsert(mp: Map[String, Int], li: List[(Long, String)]): Unit = {
    val tail = bulkInsert10000(0, new StringBuilder(), mp, li)
    if (tail.nonEmpty) {
      bulkInsert(mp, tail)
    } else {
      ()
    }
  }

  @tailrec
  def bulkInsert10000(n: Int, bulk: StringBuilder, mp: Map[String, Int], li: List[(Long, String)]): List[(Long, String)] = li match {
    case head :: tail if(n < 10000) =>
      if(n != 0){ bulk.append(",") }
      bulk.append("(")
      bulk.append(head._1.toString)
      bulk.append(",'")
      bulk.append(mp(head._2).toString)
      bulk.append("')")
      bulkInsert10000(n+1, bulk, mp, tail)
    case _ =>
      globals.db.withSession { implicit s =>
        (Q.u + "insert into order_tag (order_id, tag_id) values" + bulk.result).execute
      }
      li
  }

  def init = Action {
    globals.db.withSession { implicit s =>
      val q = sql"select tag.name, tag.id from tag".as[(String, Int)]
      val mp = q.list.toMap
      println(mp)
      // val set = scala.collection.mutable.Set[String]()
      // var c = 0
      val start = System.currentTimeMillis
      // 全件取得
      // val li = order.map(o => (o.id, o.orderTags)).list.flatMap(t => t._2.split(",").map(t._1 -> _).toList)

      // bulkInsert(mp, li)
      // li.foreach(_._2.foreach { name =>
      //   c = c+1
      //   if (c % 1000 == 0) {
      //     println(c)
      //   }
      //
      //   (Q.u + "insert into tag (name) values('" + name + "') on duplicate key update name = '" + name + "'").execute
      // })
      println("elapse: " + (System.currentTimeMillis - start))
      Ok
    }
  }
}
