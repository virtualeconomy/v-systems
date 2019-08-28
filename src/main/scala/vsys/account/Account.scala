package vsys.account

import vsys.blockchain.state.ByteStr


trait Account {
  def stringRepr: String

  def bytes: ByteStr

  override def toString: String = stringRepr

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: Account => bytes == a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}
