package vsys.api.http

import org.scalacheck.Gen.{alphaNumChar, choose, listOfN, oneOf}
import org.scalacheck.{Arbitrary, Gen => G}
import scorex.crypto.encode.Base58
import vsys.api.http.leasing.{SignedLeaseCancelRequest, SignedLeaseRequest}
import vsys.blockchain.state._
import vsys.blockchain.transaction.{TransactionGen, TransactionParser}

trait RequestGen extends TransactionGen {
  val nonPositiveLong: G[Long] = choose(Long.MinValue, 0).label("non-positive value")

  val invalidBase58: G[String] = listOfN(50, oneOf(alphaNumChar, oneOf('O', '0', 'l')))
    .map(_.mkString)
    .label("invalid base58")

  val addressStrGen: G[String] = listOfN(32, Arbitrary.arbByte.arbitrary).map(b => Base58.encode(b.toArray))

  val signatureGen: G[String] = listOfN(TransactionParser.SignatureLength, Arbitrary.arbByte.arbitrary)
    .map(b => Base58.encode(b.toArray))

  val leaseReq: G[SignedLeaseRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _leaseTx <- leaseGen
    _senderProof = _leaseTx.proofs.firstCurveProof.explicitGet
  } yield SignedLeaseRequest(_senderProof.publicKey.toString, _leaseTx.amount, _leaseTx.transactionFee, _leaseTx.feeScale, _leaseTx.recipient.toString, _timestamp, _signature)

  val leaseCancelReq: G[SignedLeaseCancelRequest] = for {
    _signature <- signatureGen
    _timestamp <- ntpTimestampGen
    _cancel <- leaseCancelGen
    _senderProof = _cancel.proofs.firstCurveProof.explicitGet
  } yield SignedLeaseCancelRequest(_senderProof.publicKey.toString, _cancel.leaseId.base58, _cancel.timestamp, _signature, _cancel.transactionFee, _cancel.feeScale)
}
