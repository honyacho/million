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

  // チュートリアル用？
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

  def time[T](f: => T, str: String): T = {
    val start = System.currentTimeMillis
    val r = f
    val elapse = System.currentTimeMillis-start
    // 一応1000msでログ
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
      // クエリをビルド
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
      // クエリ実行
      val res = Q.queryNA[String](query).list
      val el = System.currentTimeMillis - st

      // 1000ms 以上だけログ(クエリも)
      if(el > 1000) {
        Logger.info("query execution: " + el + "ms")
        Logger.info(query)
      }
      Ok(time(buildReturnJson(res), "json serialize: ")).as(JSON)
    }
  }

  // MySQLに格納してあったJSON objectをレスポンス用に組み立てる
  // json系のライブラリは実行時間の無駄なので使わない
  def buildReturnJson(objs: List[String]): String = {
    val r = new StringBuilder()
    // var(笑)
    var isFirst = true
    r.append("""{"result":true,"data":[""")
    objs.foreach{o =>
      if (isFirst) { isFirst = false } else {
        r.append(""",""")
      }
      r.append(o)
    }
    r.append("""]}""")
    r.result
  }

  // クソでかクエリくんをひたすら組み立てる
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

    // 基本だけどStringBuilderを使うことはかなり重要
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

    // userについての条件があればjoinする
    if (userCompany.nonEmpty || discountRateGte.nonEmpty || discountRateLte.nonEmpty) {
      sb.append(" inner join user u on o.order_user_id = u.user_id")
      userCompany.map(v => s"u.user_company = '${v}'").foreach(conds += _)
      discountRateGte.map("u.user_discount_rate >= " + _).foreach(conds += _)
      discountRateLte.map("u.user_discount_rate <= " + _).foreach(conds += _)
    }

    // itemについての条件があればjoinする
    if (itemSupplier.nonEmpty || itemStockQuantityGte.nonEmpty || itemStockQuantityLte.nonEmpty || itemBasePriceGte.nonEmpty || itemBasePriceLte.nonEmpty || itemTagsAll.nonEmpty || itemTagsAny.nonEmpty) {
      sb.append(" inner join item i on i.item_id = o.order_item_id")
      itemSupplier.map(i => s"i.item_supplier = '${i}'").foreach(conds += _)
      itemStockQuantityGte.map("i.item_stock_quantity >= " + _).foreach(conds += _)
      itemStockQuantityLte.map("i.item_stock_quantity <= " + _).foreach(conds += _)
      itemBasePriceGte.map("i.item_base_price >=" + _).foreach(conds += _)
      itemBasePriceLte.map("i.item_base_price <=" + _).foreach(conds += _)
    }

    // 全ての元凶(tag)
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

    // anyがクソ重い
    if (tagsAny.nonEmpty) {
      sb.append(" inner join order_tag oott on oott.order_id = o.id inner join tag tt on oott.tag_id = tt.id")
      conds += ("tt.name in " + tagsAny.mkString("('", "','", "')"))
    }

    // itemのtag
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

    // 同じく重い
    if (itemTagsAny.nonEmpty) {
      sb.append(" inner join item_tag iitt on iitt.item_id = i.id inner join tag2 tt2 on iitt.tag2_id = tt2.id")
      conds += ("tt2.name in " + itemTagsAny.mkString("('", "','", "')"))
    }

    // whereを最後に付け足して
    if (conds.nonEmpty) {
      sb.append(" where ")
      sb.append(conds mkString " and ")
    }

    // limitして終了
    sb.append(" limit ")
    sb.append(limit.getOrElse(100).toString)
    sb.result
  }
}
