package vsys.utils

import java.security.SecureRandom

import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object Random {
  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = new SecureRandom().nextInt(n)
      swap(n - 1, k)
    }

    (bf(xs) ++= buf).result()
  }
}