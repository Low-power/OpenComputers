package li.cil.oc.common.event

import cpw.mods.fml.common.eventhandler.SubscribeEvent
import li.cil.oc.api.event.RobotPlaceInAirEvent
import li.cil.oc.api.network.Component
import li.cil.oc.server.component.UpgradeAngel

import scala.collection.convert.WrapAsScala._

object AngelUpgradeHandler {
  @SubscribeEvent
  def onPlaceInAir(e: RobotPlaceInAirEvent) {
    val machineNode = e.agent.machine.node
    e.setAllowed(machineNode.reachableNodes.exists {
      case component: Component if component.canBeSeenFrom(machineNode) =>
        component.host.isInstanceOf[UpgradeAngel]
      case _ => false
    })
  }
}
