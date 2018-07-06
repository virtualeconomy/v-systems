package scorex.database

import java.awt.geom.NoninvertibleTransformException


object DataType extends Enumeration {
  val ByteArray = Value(1)
  val NoType = Value(2)

  def fromByte(b: Byte): Option[DataType.Value] = {
    if (b < DataType.ByteArray.id || b > DataType.NoType.id)
      None
    else
      Some(DataType(b))
  }
}