package transaction.parser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.json._
import transaction.domain._
import transaction.utils._

object OrderBookChanges {
	private val lsfSell: BigDecimal = 131072 // see "lsfSell" flag in rippled source code

	def parseOrderbookChanges(metadata: JsValue): Future[Seq[JsValue]] =
		for {
			meta 	<- Future.successful(metadata.asOpt[MetaData])

			nodes	<- Future.successful(Utility.normalizeNodes(meta))

			listOfNormalizeNodes <- Future.successful(nodes.filter(_.entryType == "Offer"))

			parseOrderChange <- Future.sequence(listOfNormalizeNodes.map(parseOrderChange))

			groupByAddress 	 <- groupByAddress(parseOrderChange)

		} yield (groupByAddress)

	private def groupByAddress(orderChanges: List[ConvertOrderChange]): Future[Seq[JsValue]] =
		Future.successful(orderChanges.groupBy(_.account).map(js => JsArray(js._2.map(_.toJson))).toSeq)

	private def parseOrderChange(node: NormalizeNode): Future[ConvertOrderChange] =
		for {
			finalFieldsValidated <- Future.successful {
				node.finalFields.getOrElse(JsNull).asOpt[FinalFields].getOrElse(null)
			}

			newFieldsValidated 	 <- Future.successful {
				node.newFields.getOrElse(JsNull).asOpt[NewFields].getOrElse(null)
			}

			takerpays  <- parseChangeAmount(node, "TakerPays")

			takergets  <- parseChangeAmount(node, "TakerGets")

			sell       <- Future.successful {
				finalFieldsValidated match {
					case fields: FinalFields =>
						fields.Flags.getOrElse(0) != 0 && lsfSell != 0
					case _									 =>
						false
				}
			}

			account    <- Future.successful {
				finalFieldsValidated match {
					case fields: FinalFields =>
						fields.Account.getOrElse(null)

					case _ 		 							 =>
						newFieldsValidated.Account.getOrElse(null)
				}
			}

			sequence   <- Future.successful {
				finalFieldsValidated match {
					case fields: FinalFields =>
						fields.Sequence.getOrElse(BigDecimal(0))

					case _ 										 		 =>
						newFieldsValidated.Sequence.getOrElse(BigDecimal(0))
				}
			}

			quality    <- getQuality(node)

			status 		 <- parseOrderStatus(node)

			expiration <- getExpirationTime(node)

		} yield
		(ConvertOrderChange(account, takerpays, takergets, sell, sequence, quality, status, expiration))

	private def getQuality(node: NormalizeNode): Future[String] =
		for {
			takerGets <- Future.successful {
				(node.finalFields.getOrElse(JsNull) \ "TakerGets")
					.asOpt[JsValue]
					.getOrElse((node.newFields.getOrElse(JsNull) \ "TakerGets")
					.asOpt[JsValue]
					.getOrElse(JsNull))
			}

			takerPays <- Future.successful {
				(node.finalFields.getOrElse(JsNull) \ "TakerPays")
					.asOpt[JsValue]
					.getOrElse((node.newFields.getOrElse(JsNull) \ "TakerPays")
					.asOpt[JsValue]
					.getOrElse(JsNull))
			}

			takerGetsCurrency <- Future.successful {
				takerGets.asOpt[TakerJson].map(_.currency).getOrElse("XRP")
			}

			takerPaysCurrency <- Future.successful {
				takerPays.asOpt[TakerJson].map(_.currency).getOrElse("XRP")
			}

			bookDirectory 		<- Future.successful {
				(node.finalFields.getOrElse(JsNull) \ "BookDirectory")
					.asOpt[String]
					.getOrElse((node.newFields.getOrElse(JsNull) \ "BookDirectory")
					.asOpt[String]
					.getOrElse(null))
			}

			qualityHex <- Future.successful(bookDirectory.substring(bookDirectory.length - 16))

			quality 	 <- Quality.parseQuality(qualityHex, takerGetsCurrency, takerPaysCurrency)
		} yield (quality)

	private def getExpirationTime(node: NormalizeNode): Future[String] =
		for {
			finalFields <- Future.successful {
				(node.finalFields.getOrElse(JsNull)).asOpt[JsValue].getOrElse(JsNull)
			}

			newFields  	<- Future.successful {
				(node.newFields.getOrElse(JsNull)).asOpt[JsValue].getOrElse(JsNull)
			}

			result      <- Future.successful {
				finalFields match {
					case fields: JsValue =>
						val request : String 		 = "Expiration"
						val field   : BigDecimal = {
							(fields \ request).asOpt[BigDecimal]
								.getOrElse((newFields \ request)
								.asOpt[BigDecimal]
								.getOrElse(BigDecimal(0)))
						}

						if (field == 0) "null"
						else rippleToUnixTimestamp(field).toString

					case other => "undefined"
				}
			}
		} yield (result)

	private def rippleToUnixTimestamp(rpepoch: BigDecimal): BigDecimal =
		((rpepoch + 946684800) * 1000)

	private def parseChangeAmount(node: NormalizeNode, `type`: String): Future[JsValue] = {
	  var status = parseOrderStatus(node)

	  status.map(_ match {
	  	case "cancelled" =>
	  		val finalFields = (node.finalFields.getOrElse(JsNull) \ `type`)

	  		(finalFields.asOpt[String].getOrElse(finalFields.asOpt[TakerJson].getOrElse(null)))
		  		match {
			  		case js  : TakerJson => Utility.parseCurrencyAmount(js.toJson)

						case str : String    => Utility.parseCurrencyAmount(str)

						case _ 						 	 => JsNull
	  			}

	  	case "created" =>
	  		val newFields = (node.newFields.getOrElse(JsNull) \ `type`)

	  		(newFields.asOpt[String].getOrElse(newFields.asOpt[TakerJson].getOrElse(null)))
		  		match {
			  		case js  : TakerJson => Utility.parseCurrencyAmount(js.toJson)

						case str : String    => Utility.parseCurrencyAmount(str)

						case _ 						 	 => JsNull
	  			}

			case _ =>
				val newFields 	: JsLookupResult = (node.newFields.getOrElse(JsNull) \ `type`)
				val previous  	: JsLookupResult = (node.previousFields.getOrElse(JsNull) \ `type`)
	  		val finalAmount : JsValue 		   = {
	  			(newFields.asOpt[String].getOrElse(newFields.asOpt[TakerJson].getOrElse(null)))
			  		match {
				  		case js   : TakerJson  		 => Utility.parseCurrencyAmount(js.toJson)

							case str  : String     		 => Utility.parseCurrencyAmount(str)

							case _ 		 				 	  		 => JsNull
		  			}
	  		}
	  		val prevAmount  : JsValue 		 	 = {
	  			(previous.asOpt[String].getOrElse(previous.asOpt[TakerJson].getOrElse(null)))
			  		match {
				  		case js   : TakerJson    	 => Utility.parseCurrencyAmount(js.toJson)

							case str  : String       	 => Utility.parseCurrencyAmount(str)

							case _ 						 	     	 => JsNull
		  			}
	  		}
	  		// Calculate Delta Amount..
				val calculatedDelta: String  	 	 = calculateDelta(finalAmount, prevAmount)

				Json.parse(s"""${ finalAmount }, value: ${ calculatedDelta }""")
	  })
	}

	private def calculateDelta(finalAmount: JsValue, previousAmount: JsValue): String = {
		finalAmount match {
			case amount: JsValue =>
				(	BigDecimal((finalAmount 	 \ "value").as[String]) -
					BigDecimal((previousAmount \ "value").as[String])
				).abs.toString

			case _ 							=> null
		}
	}

	private def parseOrderStatus(node: NormalizeNode): Future[String] = {
		Future.successful {
			(node.diffType) match {
				case "CreatedNode"  	=> "created"

				case "ModifiedNode" 	=> "partially-filled"

				case "DeletedNode"  	=>
					try {
						(node.previousFields.getOrElse(JsNull) \ "TakerPays").as[String]
						"filled"
					} catch {
						case _: Exception => "cancelled"
					}

				case _ 								=> "undefined"
			}
		}
	}
}
