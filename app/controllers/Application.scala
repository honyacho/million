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
  val item = TableQuery[Items]

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

  def time[T](f: => T, str: String): T = {
    val start = System.currentTimeMillis
    val r = f
    val elapse = System.currentTimeMillis-start
    if (elapse > 1000) {
      println(str + elapse + "ms")
    }
    r
  }

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
      val query = time(query1(
          timeGte,
          timeLte,
          userId,
          itemId,
          quantityGte,
          quantityLte,
          orderState,
          tagsAll.filter(_.nonEmpty).map(_.split(",")).getOrElse(Array[String]()),
          tagsAny.filter(_.nonEmpty).map(_.split(",")).getOrElse(Array[String]()),

          userCompany,
          discountRateGte,
          discountRateLte,

          itemSupplier,
          itemStockQuantityGte,
          itemStockQuantityLte,
          itemBasePriceGte,
          itemBasePriceLte,
          itemTagsAll.filter(_.nonEmpty).map(_.split(",")).getOrElse(Array[String]()),
          itemTagsAny.filter(_.nonEmpty).map(_.split(",")).getOrElse(Array[String]()),
          limit
        ), "query build time: ")

      val st = System.currentTimeMillis
      val res = Q.queryNA[String](query).list
      val el = System.currentTimeMillis - st
      if(el > 1000) {
        Logger.info("query execution: " + el + "ms")
        Logger.info(query)
      }
      Ok(time(buildReturnJson(res), "json serialize: ")).as(JSON)
    }
  }

  def buildReturnJson(objs: List[String]): String = {
    val r = new StringBuilder()
    var isFirst = true
    r.append("""{"result":true,"data":[""")
    objs.foreach{o =>
      if(isFirst) {isFirst = false;} else {
        r.append(""",""")
      }
      r.append(o)
    }
    r.append("""]}""")
    r.result
  }

  def query1(
    timeGte: Option[Long],
    timeLte: Option[Long],
    userId: Option[String],
    itemId: Option[String],
    quantityGte: Option[Int],
    quantityLte: Option[Int],
    orderState: Option[String],
    tagsAll: Array[String],
    tagsAny: Array[String],

    userCompany: Option[String],
    discountRateGte: Option[Int],
    discountRateLte: Option[Int],

    itemSupplier: Option[String],
    itemStockQuantityGte: Option[Int],
    itemStockQuantityLte: Option[Int],
    itemBasePriceGte: Option[Int],
    itemBasePriceLte: Option[Int],
    itemTagsAll: Array[String],
    itemTagsAny: Array[String],

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

    if (userCompany.nonEmpty || discountRateGte.nonEmpty || discountRateLte.nonEmpty) {
      sb.append(" inner join user u on o.order_user_id = u.user_id")
      userCompany.map(v => s"u.user_company = '${v}'").foreach(conds += _)
      discountRateGte.map("u.user_discount_rate >= " + _).foreach(conds += _)
      discountRateLte.map("u.user_discount_rate <= " + _).foreach(conds += _)
    }

    if (itemSupplier.nonEmpty || itemStockQuantityGte.nonEmpty || itemStockQuantityLte.nonEmpty || itemBasePriceGte.nonEmpty || itemBasePriceLte.nonEmpty || itemTagsAll.nonEmpty || itemTagsAny.nonEmpty) {
      sb.append(" inner join item i on i.item_id = o.order_item_id")
      itemSupplier.map(i => s"i.item_supplier = '${i}'").foreach(conds += _)
      itemStockQuantityGte.map("i.item_stock_quantity >= " + _).foreach(conds += _)
      itemStockQuantityLte.map("i.item_stock_quantity <= " + _).foreach(conds += _)
      itemBasePriceGte.map("i.item_base_price >=" + _).foreach(conds += _)
      itemBasePriceLte.map("i.item_base_price <=" + _).foreach(conds += _)
    }

    itemSupplier.map(i => s"i.item_supplier = '${i}'").foreach(conds += _)
    itemStockQuantityGte.map("i.item_stock_quantity >= " + _).foreach(conds += _)
    itemStockQuantityLte.map("i.item_stock_quantity <= " + _).foreach(conds += _)
    itemBasePriceGte.map("i.item_base_price >=" + _).foreach(conds += _)
    itemBasePriceLte.map("i.item_base_price <=" + _).foreach(conds += _)

    for ((v, i) <- tagsAll.zipWithIndex) {
      val tagTable = "t" + i.toString
      val relTable = "ot" + i.toString
      sb.append(" inner join order_tag ")
      sb.append(relTable)
      sb.append(" on ")
      sb.append(relTable)
      sb.append(".order_id = o.id")
      sb.append(" inner join tag ")
      sb.append(tagTable)
      sb.append(" on ")
      sb.append(relTable)
      sb.append(".tag_id = ")
      sb.append(tagTable)
      sb.append(".id")
      conds += (s"${tagTable}.name = '${v}'")
    }
    if (tagsAny.nonEmpty) {
      sb.append(" inner join order_tag oott on oott.order_id = o.id inner join tag tt on oott.tag_id = tt.id")
      conds += ("tt.name in " + tagsAny.mkString("('", "','", "')"))
    }

    for ((v, i) <- itemTagsAll.zipWithIndex) {
      val tagTable = "2t" + i.toString
      val relTable = "it" + i.toString
      sb.append(" inner join item_tag ")
      sb.append(relTable)
      sb.append(" on ")
      sb.append(relTable)
      sb.append(".item_id = i.id")
      sb.append(" inner join tag2 ")
      sb.append(tagTable)
      sb.append(" on ")
      sb.append(relTable)
      sb.append(".tag2_id = ")
      sb.append(tagTable)
      sb.append(".id")
      conds += (s"${tagTable}.name = '${v}'")
    }
    if (itemTagsAny.nonEmpty) {
      sb.append(" inner join item_tag iitt on iitt.item_id = i.id inner join tag2 tt2 on iitt.tag2_id = tt2.id")
      conds += ("tt2.name in " + itemTagsAny.mkString("('", "','", "')"))
    }

    if (conds.nonEmpty) {
      sb.append(" where ")
      sb.append(conds mkString " and ")
    }
    sb.append(" limit ")
    sb.append(limit.getOrElse(100).toString)
    sb.result
  }

  def query2(
    userCompany: Option[String],
    discountRateGte: Option[Int],
    discountRateLte: Option[Int],
    limit: Option[Int]
  ): String = {
    val sb = new StringBuilder()
    val conds = new scala.collection.mutable.ArrayBuffer[String]()
    userCompany.map(v => s"u.user_company = '${v}'").foreach(conds += _)
    discountRateGte.map("u.user_discount_rate >= " + _).foreach(conds += _)
    discountRateLte.map("u.user_discount_rate <= " + _).foreach(conds += _)

    sb.append("select o.json from `order` o inner join user u on o.order_user_id = u.user_id")

    if (conds.nonEmpty) {
      sb.append(" where ")
      sb.append(conds mkString " and ")
    }
    sb.append(" limit ")
    sb.append(limit.getOrElse(100).toString)
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

  def query3(
    itemSupplier: Option[String],
    itemStockQuantityGte: Option[Int],
    itemStockQuantityLte: Option[Int],
    itemBasePriceGte: Option[Int],
    itemBasePriceLte: Option[Int],
    itemTagsAll: Option[String],
    itemTagsAny: Option[String],
    limit: Option[Int]
  ): String = {
    val sb = new StringBuilder()
    val conds = new scala.collection.mutable.ArrayBuffer[String]()
    itemSupplier.map(i => s"i.item_supplier = '${i}'").foreach(conds += _)
    itemStockQuantityGte.map("i.item_stock_quantity >= " + _).foreach(conds += _)
    itemStockQuantityLte.map("i.item_stock_quantity <= " + _).foreach(conds += _)
    itemBasePriceGte.map("i.item_base_price >=" + _).foreach(conds += _)
    itemBasePriceLte.map("i.item_base_price <=" + _).foreach(conds += _)
    sb.append("select o.json from `order` o inner join item i on i.item_id = o.order_item_id")
    for ((v, i) <- itemTagsAll.zipWithIndex) {
      val tagTable = "t" + i.toString
      val relTable = "it" + i.toString
      sb.append(" inner join item_tag ")
      sb.append(relTable)
      sb.append(" on ")
      sb.append(relTable)
      sb.append(".item_id = i.id")
      sb.append(" inner join tag2 ")
      sb.append(tagTable)
      sb.append(" on ")
      sb.append(relTable)
      sb.append(".tag2_id = ")
      sb.append(tagTable)
      sb.append(".id")
      conds += (s"${tagTable}.name = '${v}'")
    }
    if (itemTagsAny.nonEmpty) {
      sb.append(" inner join item_tag iitt on iitt.item_id = i.id inner join tag2 tt on iitt.tag2_id = tt.id")
      conds += ("tt.name in " + itemTagsAny.mkString("('", "','", "')"))
    }
    if (conds.nonEmpty) {
      sb.append(" where ")
      sb.append(conds mkString " and ")
    }
    sb.append(" limit ")
    sb.append(limit.getOrElse(100).toString)
    sb.result
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
        (Q.u + "insert into item_tag (item_id, tag2_id) values" + bulk.result).execute
      }
      li
  }

  def init = Action {
    globals.db.withSession { implicit s =>
      val q = sql"select tag2.name, tag2.id from tag2".as[(String, Int)]
      val mp = q.list.toMap
      // println(mp)
      val set = scala.collection.mutable.Set[String]()
      // var c = 0
      val start = System.currentTimeMillis
      // 全件取得
      //val li = item.map(o => (o.id, o.itemTags)).list.flatMap(t => t._2.split(",").map(t._1 -> _).toList)

      //bulkInsert(mp, li)
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
