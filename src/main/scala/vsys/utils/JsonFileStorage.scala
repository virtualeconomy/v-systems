package vsys.utils

import javax.crypto.{Cipher, SecretKeyFactory}
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import java.io.{File, PrintWriter}
import java.security.NoSuchAlgorithmException
import java.security.spec.InvalidKeySpecException

import play.api.libs.json.{Json, Reads, Writes}
import scorex.crypto.encode.{Base64 => ScorexBase64}

import scala.io.{BufferedSource, Source}
import scala.util.Try

object JsonFileStorage {
  private val encoding          = "UTF-8"
  private val keySalt           = "0ba950e1-828b-4bae-9e06-faf078eb33ec"
  private val aes               = "AES"
  private val algorithm         = aes + "/ECB/PKCS5Padding"
  private val hashing           = "PBKDF2WithHmacSHA512"
  private val hashingIterations = 9999
  private val keyLength         = 256


  private def hashPassword(password: Array[Char], salt: Array[Byte], iterations: Int, keyLength: Int): Array[Byte] =
    Try {
      val skf  = SecretKeyFactory.getInstance(hashing)
      val spec = new PBEKeySpec(password, salt, iterations, keyLength)
      val key  = skf.generateSecret(spec)
      key.getEncoded
    } match {
      case Success(r) => r
      case Failure(e @ (_: NoSuchAlgorithmException | _: InvalidKeySpecException)) =>
        throw new RuntimeException(e)
    }

  def prepareKey(key: String): SecretKeySpec =
    new SecretKeySpec(hashPassword(key.toCharArray, keySalt.getBytes(encoding), hashingIterations, keyLength), aes)

  private def encrypt(key: SecretKeySpec, value: String): String = {
    val cipher: Cipher = Cipher.getInstance(algorithm)
    cipher.init(Cipher.ENCRYPT_MODE, key)
    ScorexBase64.encode(cipher.doFinal(value.getBytes(encoding)))
  }

  private def decrypt(key: SecretKeySpec, encryptedValue: String): String = {
    val cipher: Cipher = Cipher.getInstance(algorithm)
    cipher.init(Cipher.DECRYPT_MODE, key)
    new String(cipher.doFinal(ScorexBase64.decode(encryptedValue)))
  }

  def save[T](value: T, path: String, key: Option[SecretKeySpec])(implicit w: Writes[T]): Unit = {
    val folder = new File(path).getParentFile
    if (!folder.exists())
      folder.mkdirs()
    val file: Option[PrintWriter] = Option(new PrintWriter(path))
    Try {
      ultimately {
        file.foreach(_.close())
      } {
        file.foreach {
          val json = Json.toJson(value).toString()
          val data = key.fold(json)(k => encrypt(k, json))
          _.write(data)
        }
      }
    }
  }

  def save[T](value: T, path: String)(implicit w: Writes[T]): Unit =
    save(value, path, None)

  def load[T](path: String, key: Option[SecretKeySpec] = None)(implicit r: Reads[T]): T = {
    val file: Option[BufferedSource] = Option(Source.fromFile(path))
    Try {
      ultimately {  file.foreach(_.close()) } {
        file.foreach(f => {
          val data = f.mkString
          Json.parse(key.fold(data)(k => decrypt(k, data))).as[T]
        })
      }
    }
  }

  def load[T](path: String)(implicit r: Reads[T]): T =
    load(path, None)
}
