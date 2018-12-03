package vsys.fee

case class ResourcePricingBlockData(computationUnitCost: Long, storageUnitCost: Long, memoryUnitCost: Long, randomIOUnitCost: Long, sequentialIOUnitCost: Long)

// random io unit cost: times to access
// sequential io unit cost: kb
// computation unit cost: sign a signature cost