package vsys.blockchain.history

import org.iq80.leveldb.DB
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction.ValidationError
import vsys.db.{CheckpointCodec, PropertiesStorage, SubStorage}
import vsys.network.Checkpoint
import vsys.settings.CheckpointsSettings
import vsys.utils.crypto.EllipticCurveImpl

class CheckpointServiceImpl(db: DB, settings: CheckpointsSettings)
    extends SubStorage(db, "checkpoints")
    with PropertiesStorage
    with CheckpointService {

  private val CheckpointProperty = "checkpoint"

  override def get: Option[Checkpoint] = getProperty(CheckpointProperty).flatMap(b => CheckpointCodec.decode(b).toOption.map(r => r.value))


  override def set(cp: Checkpoint): Either[ValidationError, Unit] = for {
    _ <- Either.cond(!get.forall(_.signature sameElements cp.signature), (), GenericError("Checkpoint already applied"))
    _ <- Either.cond(EllipticCurveImpl.verify(cp.signature, cp.toSign, settings.publicKey.arr),
      putProperty(CheckpointProperty, CheckpointCodec.encode(cp), None),
      GenericError("Invalid checkpoint signature"))
  } yield ()

}