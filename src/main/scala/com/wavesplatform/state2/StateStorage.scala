package com.wavesplatform.state2

import com.google.common.primitives.Ints
import scorex.account.Address
import org.iq80.leveldb.{DB, WriteBatch}

import vsys.db.{Storage, SubStorage}
import vsys.db.StateMap

class StateStorage private(db: DB) extends Storage(db){

  import StateStorage._

  private val variables: StateMap[String, Int] = new StateMap(db, "variables")

  private def setPersistedVersion(version: Int) = variables.put(stateVersion, version)

  private def persistedVersion: Option[Int] = variables.get(stateVersion)

  def getHeight: Int = variables.get(heightKey).get

  def setHeight(i: Int, batchOpt: Option[WriteBatch] = None): Unit = variables.put(heightKey, i, batchOpt)

  if (variables.get(heightKey).isEmpty) setHeight(0)

  val addressList: StateMap[Int, String] = new StateMap(db, "addressList")

  val addressToID: StateMap[String, Int] = new StateMap(db, "addressToID")

  def setSlotAddress(i: Int, add: String, batchOpt: Option[WriteBatch] = None):Unit = {
    var batch: Option[WriteBatch] = batchOpt
    if (batchOpt.isEmpty) batch = createBatch()
    addressList.put(i, add, batch)
    addressToID.put(add, i, batch)
    if (batchOpt.isEmpty) commit(batch)
  }

  def getSlotAddress(i: Int): Option[String] = addressList.get(i)

  def releaseSlotAddress(i: Int, batchOpt: Option[WriteBatch] = None): Unit = {
    var batch: Option[WriteBatch] = batchOpt
    if (batchOpt.isEmpty) batch = createBatch()
    addressToID.remove(addressList.get(i).get, batch)
    addressList.remove(i, batch)
    if (batchOpt.isEmpty) commit(batch)
  }

  def addressToSlotID(add: String): Option[Int] = addressToID.get(add)

  def getEffectiveSlotAddressSize: Int = addressList.size()

  val transactions: StateMap[ByteStr, (Int, Array[Byte])] = new StateMap(db, "transactions", DataTypes.byteStr, DataTypes.transactions)

  val portfolios: StateMap[ByteStr, (Long, (Long, Long), Map[Array[Byte], Long])] = new StateMap(db, "portfolios", DataTypes.byteStr, DataTypes.portfolios)

  val assets: StateMap[ByteStr, (Boolean, Long)] = new StateMap(db, "assets", DataTypes.byteStr, DataTypes.assets)

  val accountTransactionIds: StateMap[AccountIdxKey, ByteStr] = new StateMap(db, "accountTransactionIds", valueType=DataTypes.byteStr)

  val accountTransactionsLengths: StateMap[ByteStr, Int] = new StateMap(db, "accountTransactionsLengths", keyType=DataTypes.byteStr)

  val balanceSnapshots: StateMap[AccountIdxKey, (Int, Long, Long, Long)] = new StateMap(db, "balanceSnapshots", valueType=DataTypes.balanceSnapshots)

  val aliasToAddress: StateMap[String, ByteStr] = new StateMap(db, "aliasToAddress", valueType=DataTypes.byteStr)

  val orderFills: StateMap[ByteStr, (Long, Long)] = new StateMap(db, "orderFills", keyType=DataTypes.byteStr, valueType=DataTypes.orderFills)

  val leaseState: StateMap[ByteStr, Boolean] = new StateMap(db, "leaseState", keyType=DataTypes.byteStr)

  val lastBalanceSnapshotHeight: StateMap[ByteStr, Int] = new StateMap(db, "lastUpdateHeight", keyType=DataTypes.byteStr)

  val lastBalanceSnapshotWeightedBalance: StateMap[ByteStr, Long] = new StateMap(db, "lastUpdateWeightedBalance", keyType=DataTypes.byteStr)

  val contracts: StateMap[String, (Boolean, ByteStr, String)] = new StateMap(db, "contracts")

  // only support Entry.bytes, in case later we want to support different types and not sure how to serialize here?
  val dbEntries: StateMap[ByteStr, ByteStr] = new StateMap(db, "dbEntries", keyType=DataTypes.byteStr, valueType=DataTypes.byteStr)

  override def removeEverything(batchOpt: Option[WriteBatch] = None): Unit = {
    var batch: Option[WriteBatch] = batchOpt
    if (batchOpt.isEmpty) batch = createBatch()
    new SubStorage(db, "states").removeEverything(batch)
    setHeight(0, batch)
    if (batchOpt.isEmpty) commit(batch)
  }

}

object StateStorage {

  private val Version = 1

  private val heightKey = "height"
  private val stateVersion = "stateVersion"

  private def validateVersion(ss: StateStorage): Boolean =  
    ss.persistedVersion match { 
      case None =>  
        ss.setPersistedVersion(Version)
        true  
      case Some(v) => v == Version
    }

  def apply(db: DB, dropExisting: Boolean): StateStorage = {
    val ss = new StateStorage(db)
    if (dropExisting || !validateVersion(ss)) {
      ss.removeEverything()
      new StateStorage(db)
    }
    else ss
  }

  type AccountIdxKey = Array[Byte]

  def accountIndexKey(acc: Address, index: Int): AccountIdxKey = acc.bytes.arr ++ Ints.toByteArray(index)
}
