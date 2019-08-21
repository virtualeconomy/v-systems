package vsys.api.http
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.transaction.{ValidationError, TransactionGen}
import vsys.blockchain.transaction.assets.exchange.Order
import vsys.api.http.apiErrors

import org.scalacheck.{Arbitrary, Gen}
import vsys.account.PrivateKeyAccount

import akka.http.scaladsl.model.{StatusCode, StatusCodes}

import vsys.account.{Alias, Address}

class ApiErrorSpec extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {
  //AliasNotExists(tx)
  property("ApiError should be thrown if alias not exist") {
    val alias = Alias.fromString("alias:T:sawesha").right.get
    val err = ValidationError.AliasNotExists(alias)
    val result = ApiError.fromValidationError(err)
    assert(result.id == 302 && result.code == StatusCodes.NotFound)
  }
  //CustomValidationError("UnsupportedTransactionType")
  property("UnsupportedTransactionType should result in CustomValidationError with msg 'UnsupportedTransactionType'") {
    val err = ValidationError.UnsupportedTransactionType
    val result = ApiError.fromValidationError(err)
    assert(result.id == 199 && result.message == "UnsupportedTransactionType")
  }

  //ValidationError.AccountBalanceError(errs)  CustomValidationError(errs.values.mkString(", "))
  property("AccountBalanceError should get err which value is err message list"){
    forAll(accountGen, accountGen) { (account1, account2) => 
      var errs: Map[Address, String] = Map()
      val addr1 = account1.toAddress
      val addr2 = account2.toAddress
      errs += (addr1 -> "No enough money for output")
      errs += (addr2 -> "Account is locked")
      val err = ValidationError.AccountBalanceError(errs)
      val result = ApiError.fromValidationError(err)
      assert(result.message == "No enough money for output, Account is locked")
    }
  }


  //CustomValidationError(m)
  property("OrderValidationError should cause CustomValidationError which specified msg") {
    forAll(orderGen) { order =>
      val testOrder = Order.parseBytes(order.bytes).get
      val err = ValidationError.OrderValidationError(testOrder, "order validation throw error")
      val result = ApiError.fromValidationError(err)
      assert(result.id == 199 && result.message == "order validation throw error")
    }
  }

  //Mistiming(err)
  property("Mistiming should cause ApiError.Mistiming eror") {

    val err = ValidationError.Mistiming("Mistiming error")
    val result = ApiError.fromValidationError(err)
    assert(result.id == Mistiming.Id && result.message == "Mistiming error")
  }

  // InvalidDbDataType(err)
  property("DbDataTypeError should cause error which id is 504 and specified message ") {
    val dbDataType = "SuperNode"
    val err = ValidationError.DbDataTypeError(dbDataType) 
    val result = ApiError.fromValidationError(err)
    //invalid database datatype $datatype
    assert(result.id == 504 && result.message == "invalid database datatype SuperNode")
  }

  //InvalidFeeScale(errFeeScale) InvalidFeeScale
  property("WrongFeeScale should cause err which id is 114") {
    forAll(feeScaleGen) { (feeScale: Short) => 
      val err = ValidationError.WrongFeeScale(feeScale) 
      val result = ApiError.InvalidFeeScale(err)
      assert(result.id == 114 && result.message == s"Validation failed for wrong fee scale $feeScale.")
    }
  }

  //InvalidSlotId(errSlotId)
  property("InvalidSlotId should cause err which id is 116") {
    forAll (slotidGen) { slotid => 
      val err = ValidationError.InvalidSlotId(slotid) 
      val result = ApiError.InvalidSlotId(err)
      assert(result.id == 116 && result.message == s"slot id: $slotId invalid.")
    }
  }

  //InvalidMintingReward(errMintingReward)

  //TooLongDbEntry(actualLength, maxLength)

  //InvalidUTF8String(field)

  //Mistiming(errorMessage)
}