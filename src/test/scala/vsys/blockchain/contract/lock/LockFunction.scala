package vsys.blockchain.contract.lock

import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.DataType

trait LockFunction {

  private val lockId: Short = 0

  val lockDataType: Array[Byte] = Array(DataType.Timestamp.id.toByte)

  val lockWrongDataType: Array[Byte] = Array(DataType.Amount.id.toByte)

  val lockTimeMap: StateMap = StateMap(1.toByte, DataType.Address.id.toByte, DataType.Timestamp.id.toByte)

  val lockFunctionOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(1.toByte),
    cdbvrMapGetOrDefault ++ Array(lockTimeMap.index, 1.toByte, 2.toByte),
    compareGreaterEqual ++ Array(0.toByte, 2.toByte, 3.toByte),
    assertTrue ++ Array(3.toByte),
    cdbvMapSet ++ Array(lockTimeMap.index, 1.toByte, 0.toByte)
  )

  val lockWrongFunctionOpcs: Seq[Array[Byte]] = Seq(
    Array(2.toByte, 0.toByte) ++ Array(1.toByte),
    cdbvrMapGetOrDefault ++ Array(lockTimeMap.index, 1.toByte, 2.toByte),
    compareGreaterEqual ++ Array(0.toByte, 2.toByte, 3.toByte),
    assertTrue ++ Array(3.toByte),
    cdbvMapSet ++ Array(lockTimeMap.index, 1.toByte, 0.toByte)
  )

  val nonReturnType: Array[Byte] = Array[Byte]()
  val publicFuncType: Byte = 0

  lazy val wrongDataFunc: Seq[Array[Byte]] = Seq(getFunctionBytes(lockId, publicFuncType, nonReturnType, lockWrongDataType, lockFunctionOpcs))
  lazy val wrongOpcFunc: Seq[Array[Byte]] = Seq(getFunctionBytes(lockId, publicFuncType, nonReturnType, lockDataType, lockWrongFunctionOpcs))

}
