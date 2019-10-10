package vsys.blockchain.transaction

import org.scalacheck.Gen.{alphaLowerChar, frequency, numChar}
import org.scalacheck.{Arbitrary, Gen}
import vsys.account.PublicKeyAccount._
import vsys.account._
import vsys.blockchain.consensus.SPoSCalc._
import vsys.blockchain.contract.{Contract, ContractPermitted, DataEntry, DataType => ContractDataType}
import vsys.blockchain.database.{DataType, Entry}
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import vsys.blockchain.transaction.{ProcessedTransaction, TransactionStatus}
import vsys.settings.TestFunctionalitySettings
import vsys.utils.NTP

trait TransactionGen {

  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.listOfN(length, Arbitrary.arbitrary[Byte]).map(_.toArray)

  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val bytes64gen: Gen[Array[Byte]] = byteArrayGen(64)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = for {
    length <- Gen.chooseNum(minSize, maxSize)
    bytes <- byteArrayGen(length)
  } yield bytes

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray) }
  }

  def genSeqBoundedBytes(minSize: Int, maxSize: Int): Gen[Seq[Array[Byte]]] = for {
    length <- Gen.chooseNum(minSize, maxSize)
    bytes <- byteArrayGen(length)
  } yield Seq(bytes)

  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(NTP.correctedTime() - _)

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))

  val contractAccountGen: Gen[ContractAccount] = Gen.const(ContractAccount.fromId(ByteStr(bytes32gen.sample.get)))

  val dbkeySymbolChar: Gen[Char] = Gen.oneOf('.','@', '_', '-')

  val invalidUtf8Char: Gen[Char] = Gen.oneOf('\uD800','\uD801', '\uD802')

  val validAlphabetGen: Gen[Char] = frequency((1, numChar), (1, dbkeySymbolChar), (9, alphaLowerChar))

  val entryDataStringGen: Gen[String] = for {
    length <- Gen.chooseNum(1, Entry.maxLength)
    chars <- Gen.listOfN(length, validAlphabetGen)
  } yield chars.mkString

  val validDbKeyStringGen: Gen[String] = for {
    length <- Gen.chooseNum(DbPutTransaction.MinDbKeyLength, DbPutTransaction.MaxDbKeyLength)
    dbKeyChars <- Gen.listOfN(length, validAlphabetGen)
  } yield dbKeyChars.mkString

  val validDescStringGen: Gen[String] = for {
    length <- Gen.chooseNum(RegisterContractTransaction.MinDescriptionSize, RegisterContractTransaction.MaxDescriptionSize)
    dbKeyChars <- Gen.listOfN(length, validAlphabetGen)
  } yield dbKeyChars.mkString

  val invalidLengthDbKeyStringGen: Gen[String] = for {
    length <- Gen.chooseNum(DbPutTransaction.MaxDbKeyLength + 1, DbPutTransaction.MaxDbKeyLength * 2)
    dbKeyChars <- Gen.listOfN(length, validAlphabetGen)
  } yield dbKeyChars.mkString

  val addressGen: Gen[Address] = accountGen.map(PublicKeyAccount.toAddress(_))
  val mintingAddressGen: Gen[Address] = accountGen.map(PublicKeyAccount.toAddress(_))

  def otherAccountGen(candidate: PrivateKeyAccount): Gen[PrivateKeyAccount] = accountGen.flatMap(Gen.oneOf(candidate, _))

  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue / 100)
  val positiveIntGen: Gen[Int] = Gen.choose(1, Int.MaxValue / 100)
  val positiveShortGen: Gen[Short] = Gen.choose(1, Short.MaxValue)
  val smallFeeGen: Gen[Long] = Gen.choose(1, 10000000000L)
  val feeScaleGen: Gen[Short] = Gen.const(100)
  val slotidGen: Gen[Int] = Gen.choose(0, TestFunctionalitySettings.Enabled.numOfSlots - 1)
  val attachmentGen: Gen[Array[Byte]] = genBoundedBytes(0, PaymentTransaction.MaxAttachmentSize)
  val txStatusGen: Gen[TransactionStatus.Value] = Gen.const(TransactionStatus.Success)
  val entryGen: Gen[Entry] = for {
    data: String <- entryDataStringGen
  } yield Entry.buildEntry(data, DataType.ByteArray).explicitGet()
  val dataEntryGen: Gen[Seq[DataEntry]] = accountGen.map(x => Seq(DataEntry(x.bytes.arr, ContractDataType.Address)))

  val invalidUtf8StringGen: Gen[String] = for {
    data <- Gen.listOfN(2, invalidUtf8Char)
  } yield data.mkString

  val contractContentGen: Gen[String] = for {
    length <- Gen.chooseNum(1, 1000)
    contentStr <- Gen.listOfN(length, validAlphabetGen)
  } yield contentStr.mkString
  val contractGen: Gen[Contract] = ContractPermitted.contract

  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - 100)

  val mintingAmountGen: Gen[Long] = Gen.const(MintingReward)

  val paymentGen: Gen[PaymentTransaction] = for {
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    tx <- paymentGeneratorP(sender, recipient)
  } yield tx

  val selfPaymentGen: Gen[PaymentTransaction] = accountGen.flatMap(acc => paymentGeneratorP(acc, acc))

  def paymentGeneratorP(sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] =
    timestampGen.flatMap(ts => paymentGeneratorP(ts, sender, recipient))

  def paymentGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] = for {
    amount: Long <- positiveLongGen
    fee: Long <- smallFeeGen
    feeScale: Short <- feeScaleGen
    attachment <- attachmentGen
  } yield PaymentTransaction.create(sender, recipient, amount, fee, feeScale, timestamp, attachment).explicitGet()


  private val leaseParamGen = for {
    sender <- accountGen
    amount <- positiveLongGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    timestamp <- timestampGen
    recipient <- accountGen
  } yield (sender, amount, fee, feeScale, timestamp, recipient)

  val leaseAndCancelGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, feeScale, timestamp, recipient) <- leaseParamGen
    lease = LeaseTransaction.create(sender, amount, fee, feeScale, timestamp, recipient).explicitGet()
    cancelFee <- smallFeeGen
    feeScale2: Short <- feeScaleGen
  } yield (lease, LeaseCancelTransaction.create(sender, lease.id, cancelFee, feeScale2, timestamp + 1).explicitGet())

  def leaseAndCancelGeneratorP(leaseSender: PrivateKeyAccount, recipient: Address, ts: Long = 0): Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (_, amount, fee, feeScale, times, _) <- leaseParamGen
    timestamp: Long = if (ts > 0) ts else times
    lease = LeaseTransaction.create(leaseSender, amount, fee, feeScale, timestamp, recipient).explicitGet()
    fee2 <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    unlease = LeaseCancelTransaction.create(leaseSender, lease.id, fee2, feeScale2, timestamp + 1).explicitGet()
  } yield (lease, unlease)

  val twoLeasesGen: Gen[(LeaseTransaction, LeaseTransaction)] = for {
    (sender, amount, fee, feeScale, timestamp, recipient) <- leaseParamGen
    amount2 <- positiveLongGen
    recipient2: PrivateKeyAccount <- accountGen
    fee2 <- smallFeeGen
  } yield (LeaseTransaction.create(sender, amount, fee, feeScale, timestamp, recipient).explicitGet(),
    LeaseTransaction.create(sender, amount2, fee2, feeScale, timestamp + 1, recipient2).explicitGet())

  val leaseAndCancelWithOtherSenderGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, feeScale, timestamp, recipient) <- leaseParamGen
    otherSender: PrivateKeyAccount <- accountGen
    lease = LeaseTransaction.create(sender, amount, fee, feeScale, timestamp, recipient).explicitGet()
    fee2 <- smallFeeGen
    timestamp2 <- positiveLongGen
    feeScale2: Short <- feeScaleGen
  } yield (lease, LeaseCancelTransaction.create(otherSender, lease.id, fee2, feeScale2, timestamp2).explicitGet())

  val leaseGen: Gen[LeaseTransaction] = leaseAndCancelGen.map(_._1)
  val leaseCancelGen: Gen[LeaseCancelTransaction] = leaseAndCancelGen.map(_._2)

  val mintingGen: Gen[MintingTransaction] = for {
    recipient: Address <- mintingAddressGen
    timestamp: Long <- positiveLongGen
    amount: Long <- mintingAmountGen
    currentBlockHeight: Int <- positiveIntGen
  } yield MintingTransaction.create(recipient, amount, timestamp, currentBlockHeight).explicitGet()

  def mintingGeneratorP(recipient: Address, currentBlockHeight: Int): Gen[MintingTransaction] =
    timestampGen.flatMap(ts => mintingGeneratorP(ts, recipient, currentBlockHeight))

  def mintingGeneratorP(timestamp: Long, recipient: Address, currentBlockHeight: Int): Gen[MintingTransaction] = for {
    amount: Long <- mintingAmountGen
  } yield MintingTransaction.create(recipient, amount, timestamp, currentBlockHeight).explicitGet()

  val dbPutGen: Gen[DbPutTransaction] = for {
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    dbKey: String <- validDbKeyStringGen
    entry: Entry <- entryGen
    fee: Long <- smallFeeGen
    feeScale: Short <- feeScaleGen //set to 100 in this version
  } yield DbPutTransaction.create(sender, dbKey, entry, fee * 10, feeScale, timestamp).explicitGet()

  def dbPutGeneratorP(timestamp: Long, sender: PrivateKeyAccount, fee: Long): Gen[DbPutTransaction] = for {
    dbKey: String <- validDbKeyStringGen
    entry: Entry <- entryGen
    feeScale: Short <- feeScaleGen //set to 100 in this version
  } yield DbPutTransaction.create(sender, dbKey, entry, fee, feeScale, timestamp).explicitGet()

  val registerContractGen: Gen[RegisterContractTransaction] = for {
    sender: PrivateKeyAccount <- accountGen
    contract: Contract <- contractGen
    fee: Long <- smallFeeGen
    timestamp: Long <- positiveLongGen
    feeScale: Short <- feeScaleGen
    dataStack: Seq[DataEntry] <- dataEntryGen
    description <- validDescStringGen
  } yield RegisterContractTransaction.create(sender, contract, dataStack, description, fee, feeScale, timestamp).explicitGet()

  val executeContractGen: Gen[ExecuteContractFunctionTransaction] = for {
    sender: PrivateKeyAccount <- accountGen
    contract: Contract <- contractGen
    fee: Long <- smallFeeGen
    timestamp: Long <- positiveLongGen
    feeScale: Short <- feeScaleGen
    dataStack: Seq[DataEntry] <- dataEntryGen
    description <- validDescStringGen
    contractTx = RegisterContractTransaction.create(sender, contract, dataStack, description, fee, feeScale, timestamp).explicitGet()
    otherSender: PrivateKeyAccount <- accountGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    timestamp2: Long <- positiveLongGen
    funcIdx: Short <- positiveShortGen
    data: Seq[DataEntry] <- dataEntryGen
    description <- genBoundedString(0, RegisterContractTransaction.MaxDescriptionSize)
  } yield ExecuteContractFunctionTransaction.create(otherSender, contractTx.contractId, funcIdx, data, description, fee2, feeScale2, timestamp2).explicitGet()

  val contendSlotsGen: Gen[ContendSlotsTransaction] = for {
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    slotId: Int <- slotidGen
    feeAmount <- smallFeeGen
    feeScale: Short <- feeScaleGen
  } yield ContendSlotsTransaction.create(sender, slotId/SlotGap * SlotGap, feeAmount * 50000, feeScale, timestamp).explicitGet()

  def contendGeneratorP(sender: PrivateKeyAccount, slotId: Int): Gen[ContendSlotsTransaction] =
    timestampGen.flatMap(ts => contendGeneratorP(ts, sender, slotId))

  def contendGeneratorP(timestamp: Long, sender: PrivateKeyAccount, slotId: Int): Gen[ContendSlotsTransaction] = for {
    fee: Long <- smallFeeGen
    feeScale: Short <- feeScaleGen
  } yield ContendSlotsTransaction.create(sender, slotId, fee * 50000, feeScale, timestamp).explicitGet()

  val releaseSlotsGen: Gen[ReleaseSlotsTransaction] = for {
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    slotId: Int <- slotidGen
    feeAmount <- smallFeeGen
    feeScale: Short <- feeScaleGen
  } yield  ReleaseSlotsTransaction.create(sender, slotId/SlotGap * SlotGap, feeAmount, feeScale, timestamp).explicitGet()

  def releaseGeneratorP(sender: PrivateKeyAccount, slotId: Int): Gen[ReleaseSlotsTransaction] =
    timestampGen.flatMap(ts => releaseGeneratorP(ts, sender, slotId))

  def releaseGeneratorP(timestamp: Long, sender: PrivateKeyAccount, slotId: Int): Gen[ReleaseSlotsTransaction] = for {
    fee: Long <- smallFeeGen
    feeScale: Short <- feeScaleGen
  } yield ReleaseSlotsTransaction.create(sender, slotId, fee, feeScale, timestamp).explicitGet()

  val randomProvenTransactionGen: Gen[ProvenTransaction] = (for {
    pm <- paymentGen
    ls <- leaseGen
    lc <- leaseCancelGen
    ct <- contendSlotsGen
    rl <- releaseSlotsGen
    tx <- Gen.oneOf(pm, ls, ct, rl, lc)
  } yield tx).label("random proven transaction")

  def randomProvenTransactionsGen(count: Int): Gen[Seq[ProvenTransaction]] = for {
    transactions <- Gen.listOfN(count, randomProvenTransactionGen)
  } yield transactions

  val genesisGen: Gen[GenesisTransaction] = accountGen.flatMap(genesisGeneratorP)

  def genesisGeneratorP(recipient: PrivateKeyAccount): Gen[GenesisTransaction] = for {
    amt <- positiveLongGen
    ts <- positiveIntGen
  } yield GenesisTransaction.create(recipient, amt, -1, ts).explicitGet()

  def randomProcessedTransactionGen: Gen[ProcessedTransaction] = for {
    status <- txStatusGen
    feeCharged <- smallFeeGen
    pm <- paymentGen
    ls <- leaseGen
    lc <- leaseCancelGen
    ct <- contendSlotsGen
    rl <- releaseSlotsGen
    mt <- mintingGen
    pt <- dbPutGen
    ext <- executeContractGen
    rgt <- registerContractGen
    tx <- Gen.oneOf(pm, ls, ct, rl, lc, mt, pt, ext, rgt)
  } yield ProcessedTransaction(status, feeCharged, tx)

}
