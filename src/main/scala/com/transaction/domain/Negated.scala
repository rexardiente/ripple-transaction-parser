package transaction.domain

object Negated {
  def apply[A](value: A): BigDecimal = {
    val negateNumber = value match {
      case num: BigDecimal => num
      case num: String     => BigDecimal(num)
      case num: Int        => BigDecimal(num)
      case num: Long       => BigDecimal(num)
      case num: Double     => BigDecimal(num)
      case num: Char       => BigDecimal(num)
      case _               => null
    }

    (negateNumber > 0) match {
      case true => (negateNumber - (negateNumber * 2))

      case false => (negateNumber + (negateNumber * 2))
    }
  }
}
