package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.{ContendSlotsTransaction, ValidationError}
import scorex.transaction.PoSCalc._
import scorex.account.Address
import com.wavesplatform.settings.FunctionalitySettings
import scala.util.Right

object ContendSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ContendSlotsTransaction): Either[ValidationError, Diff] = {
    val ap_gen = generatingBalance(s,fs,tx.sender,height)
    val op_gen = s.slotAddress(tx.slotid) match {
        //if the slot is occupied, return the generating balance, else return 0
      case Some(l) => Address.fromString(l).right.map( arr => generatingBalance(s,fs,arr,height)).getOrElse(0L)
      case None => 0L //here 0 can be changed to min contend cost
    }
    //for test
    //println(tx.slotid)
    //println(tx.sender.toAddress.address)
    //println(ap_gen)
    //println(op_gen)
    if (ap_gen > op_gen){
      // charge transaction fee and contend the slot
      //println("great")
      Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      slotids = Map(tx.slotid -> tx.sender.toAddress.address)))
    }
    else {
      //Charge the transaction fee without any modification
      //println("sorry")
      Right(Diff(height = height, tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty))))
    }
  }
}
