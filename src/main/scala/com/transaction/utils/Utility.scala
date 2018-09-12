package transaction.utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import transaction.domain._

object Utility {
  def normalizeNode(affectedNode: Node): NormalizeNode = {
    val JsaffectModifiedNode: JsValue = affectedNode.toJson

    affectedNode.asInstanceOf[Node].getClass.getSimpleName() match {
      case "ModifiedNodeObject" =>
        val modifiedNode = JsaffectModifiedNode.as[ModifiedNodeObject].ModifiedNode

        new NormalizeNode(
          "ModifiedNode",
          modifiedNode.LedgerEntryType.get,
          modifiedNode.LedgerIndex.get,
          None,
          modifiedNode.FinalFields,
          modifiedNode.PreviousFields,
          modifiedNode.PreviousTxnID)

      case "DeletedNodeObject"  =>
        val deletedNode = JsaffectModifiedNode.as[DeletedNodeObject].DeletedNode

        new NormalizeNode(
          "DeletedNode",
          deletedNode.LedgerEntryType.getOrElse(""),
          deletedNode.LedgerIndex.getOrElse(""),
          None,
          Some(deletedNode.FinalFields),
          deletedNode.PreviousFields,
          deletedNode.PreviousTxnID)

      case "CreatedNodeObject"  =>
        val createdNode = JsaffectModifiedNode.as[CreatedNodeObject].CreatedNode

        new NormalizeNode(
          "CreatedNode",
          createdNode.LedgerEntryType.get,
          createdNode.LedgerIndex.get,
          createdNode.NewFields,
          None,
          None,
          createdNode.PreviousTxnID)
    }
  }

  def normalizeNodes(metadata: Option[MetaData]): List[NormalizeNode] = {
    if(metadata.getOrElse(None) == None)
      List.empty
    else
      metadata.get.meta.AffectedNodes.map(normalizeNode)
  }

  def getAffectedAccounts(metadata: JsValue): Future[List[String]] = {
    for {
      meta <- Future.successful(metadata.asOpt[MetaData])

      node <- Future.successful {
        normalizeNodes(meta)
          .map { `type` =>
            if(`type`.diffType == "CreatedNode")
              `type`.newFields

            else
              `type`.finalFields
          }
        }

      array <- Future.successful {
        node.map(_.map{ js =>
          Json.parse(s"""[${js}]""").as[JsArray]
            .value
            .map(_.as[JsObject].value.map({ case (key, value) => key -> value.toString}))
        })
      }

      modifiedList <- Future.successful {
        array.flatten.flatMap(_.flatMap(_.map { count =>
          val issuer = (Json.parse(count._2) \ "issuer").asOpt[String].getOrElse(null)

          if (isAccountField(count._1))
            count._2.substring(1, (count._2.length - 1))

          else if(isAmountFieldAffectingIssuer(count._1) && (issuer != null))
            issuer
        }))
      }

      result <- Future.successful(modifiedList.filterNot(_.equals(())).distinct.map(_.toString))

    } yield (result)
  }

  def parseCurrencyAmount[A](currencyAmount: A): JsValue = {
    currencyAmount match {
      case js: JsValue =>
        val currency = (js \ "currency").asOpt[String]
        val issuer   = (js \ "issuer").asOpt[String]
        val value    = (js \ "value").asOpt[BigDecimal]
        CurrencyAmount(currency.getOrElse(""), issuer, value.getOrElse(0)).toJson

      case str: String =>
        CurrencyAmount("XRP", None, dropsToXRP(BigDecimal(str))).toJson

      case _ => (JsNull)
    }
  }

  def dropsToXRP(drops: BigDecimal): BigDecimal = (drops / (1000000))

  private def isAmountFieldAffectingIssuer(fieldName: String): Boolean =
    Seq("LowLimit", "HighLimit", "TakerPays", "TakerGets").contains(fieldName)

  private def isAccountField(fieldName: String): Boolean =
    Seq("Account", "Owner", "Destination", "Issuer", "Target").contains(fieldName)
}
