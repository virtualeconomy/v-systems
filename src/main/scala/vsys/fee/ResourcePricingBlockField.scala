package vsys.fee

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.block.BlockField


case class ResourcePricingBlockField(override val value: ResourcePricingBlockData)
  extends BlockField[ResourcePricingBlockData] {

  override val name: String = "resourcePricingData"

  override def bytes: Array[Byte] =
    Bytes.ensureCapacity(Longs.toByteArray(value.computationUnitCost), 8, 0) ++
      Bytes.ensureCapacity(Longs.toByteArray(value.storageUnitCost), 8, 0) ++
      Bytes.ensureCapacity(Longs.toByteArray(value.memoryUnitCost), 8, 0) ++
      Bytes.ensureCapacity(Longs.toByteArray(value.randomIOUnitCost), 8, 0) ++
      Bytes.ensureCapacity(Longs.toByteArray(value.sequentialIOUnitCost), 8, 0)



  override def json: JsObject = Json.obj(name -> Json.obj(
    "computation" -> value.computationUnitCost,
    "storage" -> value.storageUnitCost,
    "memory" -> value.memoryUnitCost,
    "randomIO" -> value.randomIOUnitCost,
    "sequentialIO" -> value.sequentialIOUnitCost
  ))
}
