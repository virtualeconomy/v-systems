package vsys.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.JsError
import vsys.account.Address
import vsys.blockchain.transaction.{ValidationError, TransactionGen}
import vsys.blockchain.state.diffs.TransactionDiffer.TransactionValidationError
import vsys.blockchain.database.Entry

class ApiErrorSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("UnsupportedTransactionType should result in CustomValidationError with msg 'UnsupportedTransactionType'") {
    val err = ValidationError.UnsupportedTransactionType
    val result = ApiError.fromValidationError(err)
    result.id should be(CustomValidationError(result.message).id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be("UnsupportedTransactionType")
  }

  property("AccountBalanceError should get err which value is err message list"){
    forAll(accountGen, accountGen) { (account1, account2) => 
      val addr1 = account1.toAddress
      val addr2 = account2.toAddress
      val errs: Map[Address, String] = Map(addr1 -> "No enough money for output", addr2 -> "Account is locked")
      val err = ValidationError.AccountBalanceError(errs)
      val result = ApiError.fromValidationError(err)
      result.id should be(CustomValidationError(result.message).id)
      result.code should be(StatusCodes.BadRequest)
      result.message should be("No enough money for output, Account is locked")
    }
  }

  property("Mistiming should cause ApiError.Mistiming eror") {
    val err = ValidationError.Mistiming("Mistiming error")
    val result = ApiError.fromValidationError(err)
    result.id should be(Mistiming.Id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be("Mistiming error")
  }

  property("DbDataTypeError should cause error which id is 504 and specified message ") {
    val dbDataType = "SuperNode"
    val err = ValidationError.DbDataTypeError(dbDataType) 
    val result = ApiError.fromValidationError(err)
    result.id should be(InvalidDbDataType(dbDataType).id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be("invalid database datatype SuperNode")
  }

  property("WrongFeeScale should cause err which id is 114") {
    val randomGen = scala.util.Random
    val feeScale = randomGen.nextInt(java.lang.Short.MAX_VALUE).toShort
    val err = ValidationError.WrongFeeScale(feeScale)
    val result = ApiError.fromValidationError(err)
    result.id should be(ToSelfError.id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be(s"Validation failed for wrong fee scale $feeScale.")
  }

  property("InvalidSlotId should cause err which id is 116") {
    val randomGen = scala.util.Random
    val slotId = randomGen.nextInt
    val err = ValidationError.InvalidSlotId(slotId)
    val result = ApiError.fromValidationError(err)
    result.id should be(InvalidSlotId(slotId).id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be(s"slot id: $slotId invalid.")
  }

  property("InvalidMintingReward") {
    val randomGen = scala.util.Random
    val errMintingReward = randomGen.nextLong
    val err = ValidationError.WrongMintingReward(errMintingReward)
    val result = ApiError.fromValidationError(err)
    result.id should be(InvalidMintingReward(errMintingReward).id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be(s"Validation failed for wrong minting reward $errMintingReward.")
  }
  
  property("TooLongDbEntry should throw error if actual length is greater than maxLength") {
    val maxLength = Entry.maxLength
    val randomGen = scala.util.Random
    val intMax = java.lang.Integer.MAX_VALUE
    val shortMax = java.lang.Short.MAX_VALUE.toInt
    val actualLength = randomGen.nextInt(intMax - shortMax) + shortMax
    val err = ValidationError.TooLongDbEntry(actualLength, maxLength) 
    val result = ApiError.fromValidationError(err)
    result.id should be(TooLongDbEntry(actualLength, maxLength).id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be(s"The data is too long for database put, the actual length " +
      s"is $actualLength, the maximun length supported for now is $maxLength")
    
  }
  property("InvalidUTF8String") {
    val err = ValidationError.InvalidUTF8String("contractDescription") 
    val result = ApiError.fromValidationError(err)
    result.id should be(InvalidUTF8String("contractDescription").id)
    result.code should be(StatusCodes.BadRequest)
    result.message should be(s"The contractDescription is not a valid utf8 string")
  }

  property("TransactionValidationError should return error according to error type TransactionValidationError contains") {
    forAll (randomProvenTransactionGen) { transaction =>
      val err = ValidationError.Mistiming("Mistiming error")
      val invalidTransacErr = TransactionValidationError(err, transaction)
      val result = ApiError.fromValidationError(invalidTransacErr)
      result.id should be(Mistiming.Id)
      result.code should be(StatusCodes.BadRequest)
      result.message should be("Mistiming error")
    }
  }

  property("Unknown should be error which has specific id, code and message") {
    val result = Unknown
    result.code should be(StatusCodes.InternalServerError)
    result.message should be("Error is unknown")
  }
 
  property("InvalidSeed should be error which has specific id, code and message") {
    val result = InvalidSeed
    result.code should be(StatusCodes.BadRequest)
    result.message should be("invalid seed")
  }
  
  property("InvalidAmount should be error which has specific id, code and message") {
    val result = InvalidAmount
    result.code should be(StatusCodes.BadRequest)
    result.message should be("invalid amount")
  }

  property("InvalidSender should be error which has specific id, code and message") {
    val result = InvalidSender
    result.code should be(StatusCodes.BadRequest)
    result.message should be("invalid sender")
  }

  property("InvalidRecipient should be error which has specific id, code and message") {
    val result = InvalidRecipient
    result.code should be(StatusCodes.BadRequest)
    result.message should be("invalid recipient")
  }

  property("InvalidPublicKey should be error which has specific id, code and message") {
    val result = InvalidPublicKey
    result.code should be(StatusCodes.BadRequest)
    result.message should be("invalid public key")
  }

  property("test api error id, message and code type") {
    val errors = List(
      InvalidNotNumber, 
      InvalidMessage,
      InvalidName,
      OverflowError,
      ToSelfError,
      MissingSenderPrivateKey,
      InvalidDbKey,
      InvalidProofBytes,
      InvalidProofLength,
      InvalidProofType ,
      InvalidContract,
      InvalidDataEntry,
      InvalidDataLength,
      invalidDbEntry,
      InvalidContractAddress,
      InvalidTokenIndex,
      BlockNotExists,
      ContractNotExists,
      TokenNotExists,
      InvalidFee,
      invalidDbNameSpace("NameSpace"),
      dbEntryNotExist("Entity", "NameSpace"),
      WalletSeedExportFailed,
      WalletNotExist,
      WalletLocked,
      WalletAlreadyExists,
      WalletAddressNotExists,
      TransactionNotExists,
      NoBalance,
      NegativeFee
    )

    errors.map((err) => {
      err.id shouldBe a [Integer]
      err.message shouldBe a [String]
      err.code shouldBe a [StatusCode]
    })
  }

  property("StateCheckFailed shouldBe error id, code and message with specific type") {
    forAll (randomProvenTransactionGen) { (transaction) => 
      val err = ValidationError.InvalidProofType 
      val invalidTransacErr = TransactionValidationError(err, transaction)
      val result = ApiError.fromValidationError(invalidTransacErr)
      result.id shouldBe StateCheckFailed(transaction, ApiError.fromValidationError(err).message).id
      result.message shouldBe a [String]
      result.code shouldBe StatusCodes.BadRequest
    }
  }

   property("ContractAlreadyDisabled shouldBe error id, code and message with specific type") {
    forAll (contractGen) { (contract) => 
      val err = ContractAlreadyDisabled(contract.toString)
      err.id shouldBe a [Integer]
      err.message shouldBe a [String]
      err.code shouldBe a [StatusCode]
    }
  }

  property("WrongTransactionJson") {
    val err = WrongTransactionJson (JsError("Expected JsString"))
    err.id shouldBe a [Integer]
    err.message shouldBe a [String]
    err.code shouldBe a [StatusCode]
  }
}
