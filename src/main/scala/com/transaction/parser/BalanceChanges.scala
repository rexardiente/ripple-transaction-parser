package transaction.parser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.json._
import play.api.libs.functional.syntax._
import transaction.utils.Utility._
import transaction.domain._

object BalanceChanges {
  private def computeBalanceChange(node: NormalizeNode): String = {
    val finalField    : String =
      (node.finalFields.getOrElse(JsNull) \ "Balance").asOpt[String].getOrElse(null)
    val previousField : String =
      (node.previousFields.getOrElse(JsNull) \ "Balance").asOpt[String].getOrElse(null)

    if (finalField != null)
      parseValue(finalField).toString

    else if (finalField != null && previousField != null)
      (parseValue(finalField) - parseValue(previousField)).toString

    else null
  }

  private def parseTrustlineQuantity(node: NormalizeNode, `type`: (NormalizeNode => String)): Seq[JsValue] = {
    /*
    * A trustline can be created with a non-zero starting balance
    * If an offer is placed to acquire an asset with no existing trustline,
    * the trustline can be created when the ofer is taken.
    */

    // the balance is always from low node's perspective..
    val value     : String  = `type`(node)
    val newFields : JsValue = node.newFields.getOrElse(JsNull)
    val fields    : JsValue = if (newFields == null) node.newFields.getOrElse(JsNull) else newFields
    val result    : JsValue = {
      if (value != null) {
        val balance = CounterParty(
          (fields \ "HighLimit" \ "issuer").asOpt[String].getOrElse(null),
          (fields \ "Balance" \ "currency").asOpt[String].getOrElse(null),
          value)

        Balance((fields \ "LowLimit" \ "issuer").asOpt[String].getOrElse(null), balance).toJson
      } else JsNull
    }

    Seq(result, flipTrustlinePerspective(result))
  }

  private def parseXRPQuantity(node: NormalizeNode, `type`: (NormalizeNode => String)): JsValue = {
    val value:          String = `type`(node)
    val newFieldsAcc:   String =
      (node.newFields.getOrElse(JsNull) \ "Account").asOpt[String].getOrElse(null)
    val finalFieldsAcc: String =
      (node.finalFields.getOrElse(JsNull) \ "Account").asOpt[String].getOrElse(newFieldsAcc)

    if (value == null)
      JsNull

    else
      new Balance(finalFieldsAcc, CounterParty("XRP", "", dropsToXRP(BigDecimal(value)).toString)).toJson
  }

  def parseFinalBalance(node: NormalizeNode): String = {
    val newFields   : JsLookupResult = (node.newFields.getOrElse(JsNull) \ "Balance")
    val finalFields : JsLookupResult = (node.finalFields.getOrElse(JsNull) \ "Balance")

    if (newFields.asOpt[JsValue].getOrElse(JsNull) != JsNull)
      newFields.as[String]

    else if (finalFields.asOpt[JsValue].getOrElse(JsNull) != JsNull)
      finalFields.as[String]

    else
      null
  }

  private def parseValue[A](value: A): BigDecimal = value match {
    case js    : CounterParty => BigDecimal(js.value)

    case str   : String       => BigDecimal(str)

    case num   : BigDecimal   => num

    case other                => BigDecimal(0)
  }

  def flipTrustlinePerspective(balance: JsValue): JsValue = {
    val quantity: Balance = balance.asOpt[Balance].getOrElse(null)

    if (quantity != null) {
      val negatedBalance : BigDecimal = Negated(quantity.balance.value)
      val address        : String     = quantity.balance.counterparty

      Balance(address, CounterParty(quantity.address, quantity.address, negatedBalance.toString)).toJson
    }

    else JsNull
  }

  def groupByAddress(balanceChanges: JsArray): Future[Seq[JsValue]] =
    for {
      grouped <- Future.successful {
        balanceChanges.asOpt[Seq[Balance]]
          .map(_.groupBy(_.address).map(_._2))
          .getOrElse(Seq.empty)
          .toSeq
      }

      balance <- Future.successful {
        if (grouped == Seq.empty)
          Seq.empty

        else
          grouped.map(arr => JsArray(arr.map(_.balance.toJson)))
      }
    } yield balance

  def parseBalanceChanges(metadata: JsValue): Future[Seq[JsValue]] =
    parseQuantities(metadata, computeBalanceChange)

  def parseFinalBalances(metadata: JsValue): Future[Seq[JsValue]] =
    parseQuantities(metadata, parseFinalBalance)

  def parseQuantities(metadata: JsValue, set: (NormalizeNode => String)):
    Future[Seq[JsValue]] =
    for {
      meta <- Future.successful(metadata.asOpt[MetaData])

      normalizeNodes <- Future.successful(normalizeNodes(meta))

      quantity <- Future.successful {
        (normalizeNodes.map { node =>
          if (node.entryType == "AccountRoot")
            Seq(parseXRPQuantity(node, set))

          else if (node.entryType == "RippleState")
            parseTrustlineQuantity(node, set)

          else Seq(JsNull)
        }.flatten)
      }

      compact <- Future.successful(quantity.filter(_ != JsNull).toSeq)

      grouped <- groupByAddress(JsArray(compact))
    } yield (grouped)
}
