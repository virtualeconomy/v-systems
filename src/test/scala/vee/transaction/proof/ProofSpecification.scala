package vee.transaction.proof

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.ValidationError
import com.wavesplatform.state2.ByteStr
import scorex.account.PublicKeyAccount
import scorex.serialization.BytesSerializable

class ProofSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("convert proof to byte and convert back") {
    val publicKey:Option[PublicKeyAccount] = PublicKeyAccount.fromBase58String("Hhg1VXGM27Rsjt5NVJgcMpcYgHn8ft6LTexpw3JHr25t").toOption
    val signature:ByteStr = ByteStr.decodeBase58("62PirdnRZ7AN3dP5Nc3zxoNt5FRhjyfYKSzDdbGQEJcbnvmkvwiARQsy54TaBfJFLDbSqmHiGFnKf7DWdbBmBiWW").get
    val proof = Proof.buildProof(publicKey, ProofType.Curve25519, signature)
    proof.map(_.bytes).map(_.arr).flatMap(Proof.fromBytes(_)) should be (proof)
  }

  property("report invalid proof type") {
    val publicKeyBytes:Array[Byte] = PublicKeyAccount.fromBase58String("Hhg1VXGM27Rsjt5NVJgcMpcYgHn8ft6LTexpw3JHr25t").toOption.get.publicKey
    val signatureBytes:Array[Byte] = ByteStr.decodeBase58("62PirdnRZ7AN3dP5Nc3zxoNt5FRhjyfYKSzDdbGQEJcbnvmkvwiARQsy54TaBfJFLDbSqmHiGFnKf7DWdbBmBiWW").get.arr
    val byteArray1:Array[Byte] = BytesSerializable.arrayWithSize(publicKeyBytes) ++ Array[Byte](0) ++ signatureBytes
    val byteArray2:Array[Byte] = BytesSerializable.arrayWithSize(publicKeyBytes) ++ Array[Byte](3) ++ signatureBytes
    val byteArray3:Array[Byte] = BytesSerializable.arrayWithSize(publicKeyBytes) ++ Array[Byte](1) ++ signatureBytes
    Proof.fromBytes(byteArray1) should be (Left(ValidationError.InvalidProofType))
    Proof.fromBytes(byteArray2) should be (Left(ValidationError.InvalidProofType))
    Proof.fromBytes(byteArray3).map(_.proofType) should be (Right(ProofType.Curve25519))
  }

}
