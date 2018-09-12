package transaction.domain

import play.api.libs.json._
import play.api.libs.functional.syntax._

object PreviousFields {
  implicit val implPreviousFields = Json.format[PreviousFields]
}
object HighLimit {
  implicit val implFinalFields = Json.format[HighLimit]
}
object LowLimit {
  implicit val implFinalFields = Json.format[LowLimit]
}
object FinalFields {
  implicit val implFinalFields = Json.format[FinalFields]
}
object NewFields {
  implicit val implNewFields = Json.format[NewFields]
}
object ModifiedNode {
  implicit val implModifiedNode = Json.format[ModifiedNode]
}
object DeletedNode {
  implicit val implDeletedNode = Json.format[DeletedNode]
}
object CreatedNode {
  implicit val implCreatedNode = Json.format[CreatedNode]
}
object ModifiedNodeObject {
  implicit val implModifiedNodeObject = Json.format[ModifiedNodeObject]
}
object DeletedNodeObject {
  implicit val implDeletedNodeObject = Json.format[DeletedNodeObject]
}
object CreatedNodeObject {
  implicit val implCreatedNodeObject = Json.format[CreatedNodeObject]
}
object Node {
  import ModifiedNodeObject._
  import DeletedNodeObject._
  import CreatedNodeObject._

  implicit val tempResultR1: Reads[Node] = {
    Json.format[ModifiedNodeObject].map(x => x: Node) or
    Json.format[DeletedNodeObject].map(x => x: Node) or
    Json.format[CreatedNodeObject].map(x => x: Node)
  }

  implicit val tempResultW = new Writes[Node] {
    def writes(node: Node): JsValue = {
      node match {
        case m: ModifiedNodeObject => Json.toJson(m)
        case m: DeletedNodeObject => Json.toJson(m)
        case m: CreatedNodeObject => Json.toJson(m)
        case _ => Json.obj("error" -> "wrong Json")
      }
    }
  }
}

object Meta {
  implicit val implMeta = Json.format[Meta]
}
object MetaData {
  implicit val implMeta = Json.format[MetaData]
}

case class HighLimit(currency: Option[String], issuer: Option[String], value: Option[String])
case class LowLimit(currency: Option[String], issuer: Option[String], value: Option[String])
case class PreviousFields(
  Balance: Option[JsValue],
  Sequence: Option[BigDecimal],
  Flags: Option[BigDecimal],
  HightLimit: Option[JsValue]) {
  def toJson = Json.toJson(this)
}
case class FinalFields (
  Balance: Option[JsValue],
  Flags: Option[BigDecimal],
  Account: Option[String],
  Amount: Option[String],
  Destination: Option[String],
  OwnerCount: Option[BigDecimal],
  Sequence: Option[BigDecimal],
  OwnerNode: Option[String],
  PublicKey: Option[String],
  HighLimit: Option[HighLimit],
  LowLimit: Option[LowLimit],
  HighNode: Option[String],
  LowNode: Option[String],
  PreviousTxnID: Option[String],
  PreviousTxnLgrSeq: Option[BigDecimal],
  SettleDelay: Option[BigDecimal],
  IndexNext: Option[String],
  IndexPrevious: Option[String],
  SourceTag: Option[BigDecimal],
  Owner: Option[String],
  RootIndex: Option[String]) {
  def toJson = Json.toJson(this)
}

case class NewFields (
  Sequence: Option[BigDecimal],
  ExchangeRate: Option[String],
  RootIndex: Option[String],
  Account: Option[String],
  TakerGetsCurrency: Option[String],
  TakerGetsIssuer: Option[String],
  TakerPaysCurrency: Option[String],
  TakerPaysIssuer: Option[String]) {
  def toJson = Json.toJson(this)
}

case class ModifiedNode (
  LedgerEntryType: Option[String],
  LedgerIndex: Option[String],
  PreviousTxnID: Option[String],
  PreviousTxnLgrSeq: Option[JsValue],
  FinalFields: Option[JsValue],
  PreviousFields: Option[JsValue]) {
  def toJson = Json.toJson(this)
}

case class DeletedNode (
  FinalFields: JsValue,
  LedgerEntryType: Option[String],
  LedgerIndex: Option[String],
  PreviousTxnID: Option[String],
  PreviousFields: Option[JsValue]) {
  def toJson = Json.toJson(this)
}

case class CreatedNode (
  LedgerEntryType: Option[String],
  LedgerIndex: Option[String],
  PreviousTxnID: Option[String],
  NewFields: Option[JsValue]) {
  def toJson = Json.toJson(this)
}

sealed trait Node {
  def toJson = Json.toJson(this)
}
case class ModifiedNodeObject(ModifiedNode: ModifiedNode)  extends Node
case class DeletedNodeObject(DeletedNode: DeletedNode)     extends Node
case class CreatedNodeObject(CreatedNode: CreatedNode)     extends Node

case class Meta(
  AffectedNodes: List[Node],
  TransactionIndex: Option[BigDecimal],
  TransactionResult: Option[String] )

case class MetaData(meta: Meta) { def toJson = Json.toJson(this) }
