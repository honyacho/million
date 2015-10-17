import play.api._
import play.api.Play.current
import play.api.db._
import scala.slick.driver.MySQLDriver.simple._

package object globals {
  val db = Database.forDataSource(DB.getDataSource())
}
