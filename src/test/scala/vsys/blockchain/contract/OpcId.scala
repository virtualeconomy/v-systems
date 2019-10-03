package vsys.blockchain.contract

import org.scalacheck.Gen
import vsys.blockchain.state.opcdiffs._
import OpcId._

trait OpcId {

  def opcGen(id: Array[Byte]): Gen[Array[Byte]] = for {
    opc <- Gen.const(id)
  } yield opc
  // unused setting/gen
  val assertGteqZeroGen: Gen[Array[Byte]] = opcGen(opcAssertGteqZero)
  val assertLteqGen: Gen[Array[Byte]] = opcGen(opcAssertLteq)
  val assertLtInt64Gen: Gen[Array[Byte]] = opcGen(opcAssertLtInt64)
  val assertGtZeroGen: Gen[Array[Byte]] = opcGen(opcAssertGtZero)
  val assertEqGen: Gen[Array[Byte]] = opcGen(opcAssertEq)
  val assertIsCallerOriginGen: Gen[Array[Byte]] = opcGen(opcAssertIsCallerOrigin)
  val assertIsSignerOriginGen: Gen[Array[Byte]] = opcGen(opcAssertIsSignerOrigin)

  val loadSingerGen: Gen[Array[Byte]] = opcGen(opcLoadSigner)
  val loadCallerGen: Gen[Array[Byte]] = opcGen(opcLoadCaller)

  val cdbvSetGen: Gen[Array[Byte]] = opcGen(opcCDBVSet)

  val cdbvrGetGen: Gen[Array[Byte]] = opcGen(opcCDBVRGet)

  val tdbNewTokenGen: Gen[Array[Byte]] = opcGen(opcTDBNewToken)
  val tdbSplitGen: Gen[Array[Byte]] = opcGen(opcTDBSplit)

  val tdbrOpcMaxGet: Gen[Array[Byte]] = opcGen(opcTDBROpcMax)
  val tdbrOpcTotalGen: Gen[Array[Byte]] = opcGen(opcTDBROpcTotal)

  val rdbaDepositGen: Gen[Array[Byte]] = opcGen(opcTDBADeposit)
  val tdbaWithdrawGen: Gen[Array[Byte]] = opcGen(opcTDBAWithdraw)
  val tbdaTransferGen: Gen[Array[Byte]] = opcGen(opcTDBATransfer)

  val tdbarBalanceGen: Gen[Array[Byte]] = opcGen(opcTDBARBalance)
}

object OpcId {
  val opcAssertGteqZero: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.GteqZeroAssert.id.toByte)
  val opcAssertLteq: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.LteqAssert.id.toByte)
  val opcAssertLtInt64: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.LtInt64Assert.id.toByte)
  val opcAssertGtZero: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.GtZeroAssert.id.toByte)
  val opcAssertEq: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.EqAssert.id.toByte)
  val opcAssertIsCallerOrigin: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.IsCallerOriginAssert.id.toByte)
  val opcAssertIsSignerOrigin: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.IsSignerOriginAssert.id.toByte)

  val opcLoadSigner: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.SignerLoad.id.toByte)
  val opcLoadCaller: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.CallerLoad.id.toByte)

  val opcCDBVSet: Array[Byte] = Array(OpcDiffer.OpcType.CDBVOpc.id.toByte, CDBVOpcDiff.CDBVType.SetCDBV.id.toByte)

  val opcCDBVRGet: Array[Byte] = Array(OpcDiffer.OpcType.CDBVROpc.id.toByte, CDBVROpcDiff.CDBVRType.GetCDBVR.id.toByte)

  val opcTDBNewToken: Array[Byte] = Array(OpcDiffer.OpcType.TDBOpc.id.toByte, TDBOpcDiff.TDBType.NewTokenTDB.id.toByte)
  val opcTDBSplit: Array[Byte] = Array(OpcDiffer.OpcType.TDBOpc.id.toByte, TDBOpcDiff.TDBType.SplitTDB.id.toByte)

  val opcTDBROpcMax: Array[Byte] = Array(OpcDiffer.OpcType.TDBROpc.id.toByte, TDBROpcDiff.TDBRType.MaxTDBR.id.toByte)
  val opcTDBROpcTotal: Array[Byte] = Array(OpcDiffer.OpcType.TDBROpc.id.toByte, TDBROpcDiff.TDBRType.TotalTDBR.id.toByte)

  val opcTDBADeposit: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.DepositTDBA.id.toByte)
  val opcTDBAWithdraw: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.WithdrawTDBA.id.toByte)
  val opcTDBATransfer: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.TransferTDBA.id.toByte)

  val opcTDBARBalance: Array[Byte] = Array(OpcDiffer.OpcType.TDBAROpc.id.toByte, TDBAROpcDiff.TDBARType.BalanceTBDAR.id.toByte)

  val opcReturnValue: Array[Byte] = Array(OpcDiffer.OpcType.ReturnOpc.id.toByte, ReturnOpcDiff.ReturnType.ValueReturn.id.toByte)
}