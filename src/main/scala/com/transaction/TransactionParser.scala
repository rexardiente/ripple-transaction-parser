package transaction

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.json._
import transaction.utils._
import transaction.domain._
import transaction.parser._

object TransactionParser {
  def parseOrderbookChanges(metaData: JsValue): Future[Seq[JsValue]] =
    OrderBookChanges.parseOrderbookChanges(metaData)

  def parseBalanceChanges(metaData: JsValue): Future[Seq[JsValue]] =
    BalanceChanges.parseBalanceChanges(metaData)

  def parseFinalBalances(metaData: JsValue): Future[Seq[JsValue]] =
    BalanceChanges.parseFinalBalances(metaData)

  def getAffectedAccounts(metaData: JsValue): Future[List[String]] =
    Utility.getAffectedAccounts(metaData)

  def parseChannelChanges(metaData: JsValue): Future[JsValue] =
    ChannelChanges.parseChannelChanges(metaData)

  def getNormalizeNodes(metaData: Option[MetaData]): List[NormalizeNode] =
    Utility.normalizeNodes(metaData)
}
