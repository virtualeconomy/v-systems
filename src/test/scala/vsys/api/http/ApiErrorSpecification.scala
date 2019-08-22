package vsys.api.http
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.transaction.{ValidationError, TransactionGen}
import vsys.blockchain.transaction.assets.exchange.Order
import vsys.blockchain.state.diffs.TransactionDiffer.TransactionValidationError
import vsys.blockchain.database.Entry
import vsys.blockchain.consensus.SPoSCalc._

import akka.http.scaladsl.model.StatusCodes

import vsys.account.{Alias, Address}

class ApiErrorSpec extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {
  property("ApiError should be thrown if alias not exist") {
    val alias = Alias.fromString("alias:T:sawesha").right.get
    val err = ValidationError.AliasNotExists(alias)
    val result = ApiError.fromValidationError(err)
    result.id should be(302)
    result.code should be(StatusCodes.NotFound)
  }
  property("UnsupportedTransactionType should result in CustomValidationError with msg 'UnsupportedTransactionType'") {
    val err = ValidationError.UnsupportedTransactionType
    val result = ApiError.fromValidationError(err)
    result.id should be(199)
    result.code should be(StatusCodes.BadRequest)
    result.message should be("UnsupportedTransactionType")
  }

  property("AccountBalanceError should get err which value is err message list"){
    forAll(accountGen, accountGen) { (account1, account2) => 
      var errs: Map[Address, String] = Map()
      val addr1 = account1.toAddress
      val addr2 = account2.toAddress
      errs += (addr1 -> "No enough money for output")
      errs += (addr2 -> "Account is locked")
      val err = ValidationError.AccountBalanceError(errs)
      val result = ApiError.fromValidationError(err)
      result.id should be(199)
      result.code should be(StatusCodes.BadRequest)
      result.message should be("No enough money for output, Account is locked")
    }
  }


  property("OrderValidationError should cause CustomValidationError which specified msg") {
    forAll(orderGen) { order =>
      val testOrder = Order.parseBytes(order.bytes).get
      val err = ValidationError.OrderValidationError(testOrder, "order validation throw error")
      val result = ApiError.fromValidationError(err)
      result.id should be(199)
      result.code should be(StatusCodes.BadRequest)
      result.message should be("order validation throw error")
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
    result.id should be(504)
    result.code should be(StatusCodes.BadRequest)
    result.message should be("invalid database datatype SuperNode")
  }

  property("WrongFeeScale should cause err which id is 114") {
    forAll(feeScaleGen) { (feeScale: Short) => 
      val err = ValidationError.WrongFeeScale(feeScale) 
      val result = ApiError.fromValidationError(err)
      result.id should be(114)
      result.code should be(StatusCodes.BadRequest)
      result.message should be(s"Validation failed for wrong fee scale $feeScale.")
    }
  }

  property("InvalidSlotId should cause err which id is 116") {
    forAll (slotidGen) { slotId => 
      val err = ValidationError.InvalidSlotId(slotId) 
      val result = ApiError.fromValidationError(err)
      result.id should be(116)
      result.code should be(StatusCodes.BadRequest)
      result.message should be(s"slot id: $slotId invalid.")
    }
  }

  //InvalidMintingReward(errMintingReward)
  property("InvalidMintingReward") {
    forAll(mintingAmountGen){ errMintingReward => 
      val err = ValidationError.WrongMintingReward(errMintingReward)
      val result = ApiError.fromValidationError(err)
      result.id should be(115)
      result.code should be(StatusCodes.BadRequest)
      result.message should be(s"Validation failed for wrong minting reward $errMintingReward.")
    }
  }
  
  //TooLongDbEntry(actualLength, maxLength)
  property("TooLongDbEntry should throw error if actual length is greater than maxLength") {
    val maxLength = Entry.maxLength
    val actualLength = maxLength + 2

    val err = ValidationError.TooLongDbEntry(actualLength, maxLength) 
    val result = ApiError.fromValidationError(err)
    result.id should be(505)
    result.code should be(StatusCodes.BadRequest)
    result.message should be(s"The data is too long for database put, the actual length " +
      s"is $actualLength, the maximun length supported for now is $maxLength")
    
  }
  property("InvalidUTF8String") {
    val err = ValidationError.InvalidUTF8String("contractDescription") 
    val result = ApiError.fromValidationError(err)
    result.id should be(118)
    result.code should be(StatusCodes.BadRequest)
    result.message should be(s"The contractDescription is not a valid utf8 string")
  }

  property("TransactionValidationError should return error according to error type TransactionValidationError contains") {
    forAll (randomTransactionGen) { transaction =>
      val err = ValidationError.Mistiming("Mistiming error")
      val invalidTransacErr = TransactionValidationError(err, transaction)
      val result = ApiError.fromValidationError(invalidTransacErr)
      result.id should be(Mistiming.Id)
      result.code should be(StatusCodes.BadRequest)
      result.message should be("Mistiming error")
    }
  }
}
