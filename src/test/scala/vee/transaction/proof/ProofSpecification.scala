package vee.transaction.proof

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.ValidationError
import com.wavesplatform.state2.ByteStr
import scorex.account.PublicKeyAccount

class ProofSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("convert proof to byte and convert back") {
    val publicKey:PublicKeyAccount = PublicKeyAccount.fromBase58String("Hhg1VXGM27Rsjt5NVJgcMpcYgHn8ft6LTexpw3JHr25t").toOption.get
    val signature:ByteStr = ByteStr.decodeBase58("62PirdnRZ7AN3dP5Nc3zxoNt5FRhjyfYKSzDdbGQEJcbnvmkvwiARQsy54TaBfJFLDbSqmHiGFnKf7DWdbBmBiWW").get
    val proof = EllipticCurve25519Proof.buildProof(publicKey, signature)
    proof.map(_.bytes).map(_.arr).flatMap(EllipticCurve25519Proof.fromBytes(_)) should be (proof)
  }

  property("report invalid proof type") {
    val publicKeyBytes:Array[Byte] = PublicKeyAccount.fromBase58String("Hhg1VXGM27Rsjt5NVJgcMpcYgHn8ft6LTexpw3JHr25t").toOption.get.publicKey
    val signatureBytes:Array[Byte] = ByteStr.decodeBase58("62PirdnRZ7AN3dP5Nc3zxoNt5FRhjyfYKSzDdbGQEJcbnvmkvwiARQsy54TaBfJFLDbSqmHiGFnKf7DWdbBmBiWW").get.arr
    val byteArray1:Array[Byte] = Array[Byte](0) ++ publicKeyBytes ++ signatureBytes
    val byteArray2:Array[Byte] = Array[Byte](3) ++ publicKeyBytes ++ signatureBytes
    val byteArray3:Array[Byte] = Array[Byte](1) ++ publicKeyBytes ++ signatureBytes
    EllipticCurve25519Proof.fromBytes(byteArray1) should be (Left(ValidationError.InvalidProofType))
    EllipticCurve25519Proof.fromBytes(byteArray2) should be (Left(ValidationError.InvalidProofType))
    EllipticCurve25519Proof.fromBytes(byteArray3).map(_.proofType) should be (Right(ProofType.Curve25519))
  }

}
