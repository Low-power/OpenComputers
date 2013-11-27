package li.cil.oc.common.tileentity

import li.cil.oc.Settings
import li.cil.oc.api.driver.Slot
import li.cil.oc.server.driver.Registry
import net.minecraft.item.ItemStack

class Case(isRemote: Boolean) extends Computer(isRemote) {
  def this() = this(false)

  // ----------------------------------------------------------------------- //

  def getInvName = Settings.namespace + "container.Case"

  def getSizeInventory = 8

  def isItemValidForSlot(slot: Int, stack: ItemStack) = (slot, Registry.driverFor(stack)) match {
    case (_, None) => false // Invalid item.
    case (0 | 1 | 2, Some(driver)) => driver.slot(stack) == Slot.Card
    case (3 | 4, Some(driver)) => driver.slot(stack) == Slot.Memory
    case (5 | 6, Some(driver)) => driver.slot(stack) == Slot.HardDiskDrive
    case (7, Some(driver)) => driver.slot(stack) == Slot.Disk
    case _ => false // Invalid slot.
  }
}