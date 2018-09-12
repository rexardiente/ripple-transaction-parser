package transaction.parser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.json._
import transaction.domain._
import transaction.utils.Utility._

object ChannelChanges {
  def parseChannelChanges(metadata: JsValue): Future[JsValue] =
    for {
      meta             <- Future.successful(metadata.asOpt[MetaData])

      normalizeNodes   <- Future.successful(normalizeNodes(meta))

      payChannel       <- Future.successful(normalizeNodes.filter(_.entryType == "PayChannel"))

      summarizePayment <- Future.successful {
        if (payChannel.length == 1)
          summarizePaymentChannel(payChannel.head)
        else
          JsNull
      }
    } yield (summarizePayment)

  private def summarizePaymentChannel(node: NormalizeNode): JsValue = {
    val prev         : JsValue =  node.previousFields.getOrElse(JsNull)
    val finalized    : JsValue = {
      if (node.diffType == "CreatedNode")
        node.newFields.getOrElse(JsNull)
      else
        node.finalFields.getOrElse(JsNull)
    }
    val finalBalance : String  = (finalized \ "Balance").asOpt[String].getOrElse("0")
    val finalAmount  : String  = (finalized \ "Amount").asOpt[String].getOrElse("0")

    new Summary(
      parsePaymentChannelStatus(node),
      // The LedgerIndex indicates the Channel ID, which is necessary to sign claims.
      node.ledgerIndex,

      // The source address that owns this payment channel.
      // This comes from the sending address of the transaction that created the channel.
      (finalized \ "Account").asOpt[String].getOrElse(null),
      // finalized.Account,

      // The destination address for this payment channel.
      // While the payment channel is open, this address is the only one that can receive
      // XRP from the channel. This comes from the Destination field of the transaction
      // that created the channel.
      (finalized \ "Destination").asOpt[String].getOrElse(null),

      // Total XRP, in drops, that has been allocated to this channel.
      // This includes XRP that has been paid to the destination address.
      // This is initially set by the transaction that created the channel and
      // can be increased if the source address sends a PaymentChannelFund transaction.
      BigDecimal(finalAmount).toString,

      // Total XRP, in drops, already paid out by the channel.
      // The difference between this value and the Amount field is how much XRP can still
      // be paid to the destination address with PaymentChannelClaim transactions.
      // If the channel closes, the remaining difference is returned to the source address.
      BigDecimal(finalBalance).toString,

      (prev \ "Amount").asOpt[String].map { amount =>
        (BigDecimal(finalAmount) - BigDecimal(amount)).toString
      }.getOrElse(null),

      (prev \ "Balance").asOpt[String].map { amount =>
        (BigDecimal(finalBalance) - BigDecimal(amount)).toString
      }.getOrElse(null),

      node.PreviousTxnID.getOrElse(null)
    ).toJson
  }

  def parsePaymentChannelStatus(node: NormalizeNode): String =
    (node.diffType.toString) match {
      case "CreatedNode"  => "created"
      case "ModifiedNode" => "modified"
      case "DeletedNode"  => "deleted"
      case _              => "undefined"
    }
}
