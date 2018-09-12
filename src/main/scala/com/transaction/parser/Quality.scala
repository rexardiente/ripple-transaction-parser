package transaction.parser

import scala.concurrent.Future
import scala.util.Either
import java.math.{BigDecimal, MathContext}

object Quality {
	def adjustQualityForXRP(
			quality: String,
			takerGetsCurrency: String,
			takerPaysCurrency: String
		): Future[String] = {

		val numeratorShift 		= { if (takerGetsCurrency == "XRP") -6 else 0 }
		val denominatorShift  = { if (takerPaysCurrency == "XRP") -6 else 0 }
		val shift 						= { numeratorShift - denominatorShift 				}

		Future.successful({
			if (shift == 0)
				new BigDecimal(quality, new MathContext(27)).toPlainString()
			else {
				if (shift < 0)
					new BigDecimal(quality, new MathContext(27)).movePointLeft(-12).toPlainString
				else
					new BigDecimal(quality, new MathContext(27)).movePointRight(-12).toPlainString
			}
		})
	}

	def parseQuality(
			qualityHex: String,
			takerGetsCurrency: String,
			takerPaysCurrency: String
		): Future[String] = {

		if(qualityHex.length == 16 ) {
			val mantissa: BigInt = BigInt(qualityHex.substring(2), 16)
			val offset  : Int  	 = Integer.parseInt(qualityHex.substring(0, 2), 16) - 100
			val quality : String = mantissa.toString + "e" + offset.toString()

			adjustQualityForXRP(quality, takerGetsCurrency, takerPaysCurrency)
		} else {
			Future.successful("undefined")
		}

	}
}
