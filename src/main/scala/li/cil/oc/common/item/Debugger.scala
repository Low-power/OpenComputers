package li.cil.oc.common.item

import li.cil.oc.OpenComputers
import li.cil.oc.api
import li.cil.oc.api.network._
import li.cil.oc.util.BlockPosition
import li.cil.oc.util.ExtendedWorld._
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.item.ItemStack
import net.minecraftforge.common.util.ForgeDirection

class Debugger(val parent: Delegator) extends Delegate {
  override def onItemUse(stack: ItemStack, player: EntityPlayer, position: BlockPosition, side: Int, hitX: Float, hitY: Float, hitZ: Float) = {
    val world = position.world.get
    player match {
      case realPlayer: EntityPlayerMP =>
        world.getTileEntity(position) match {
          case host: SidedEnvironment =>
            if (!world.isRemote) {
              Debugger.reconnect(Array(host.sidedNode(ForgeDirection.getOrientation(side))))
            }
            true
          case host: Environment =>
            if (!world.isRemote) {
              Debugger.reconnect(Array(host.node))
            }
            true
          case _ =>
            if (!world.isRemote) {
              Debugger.node.remove()
            }
            true
        }
      case _ => false
    }
  }
}

object Debugger extends Environment {
  var node = api.Network.newNode(this, Visibility.Network).create()

  override def onConnect(node: Node) {
    OpenComputers.log.info(s"[NETWORK DEBUGGER] New node in network: ${nodeInfo(node)}")
  }

  override def onDisconnect(node: Node) {
    OpenComputers.log.info(s"[NETWORK DEBUGGER] Node removed from network: ${nodeInfo(node)}")
  }

  override def onMessage(message: Message) {
    OpenComputers.log.info(s"[NETWORK DEBUGGER] Received message: ${messageInfo(message)}.")
  }

  def reconnect(nodes: Array[Node]) {
    node.remove()
    api.Network.joinNewNetwork(node)
    for (node <- nodes if node != null) {
      this.node.connect(node)
    }
  }

  private def nodeInfo(node: Node) = s"{address = ${node.address}, reachability = ${node.reachability.name}" + (node match {
    case componentConnector: ComponentConnector => componentInfo(componentConnector) + connectorInfo(componentConnector)
    case component: Component => componentInfo(component)
    case connector: Connector => connectorInfo(connector)
    case _ =>
  }) + "}"

  private def componentInfo(component: Component) = s", type = component, name = ${component.name}, visibility = ${component.visibility.name}"

  private def connectorInfo(connector: Connector) = s", type = connector, buffer = ${connector.localBuffer}, bufferSize = ${connector.localBufferSize}"

  private def messageInfo(message: Message) = s"{name = ${message.name()}, source = ${nodeInfo(message.source)}, data = [${message.data.mkString(", ")}]}"
}
