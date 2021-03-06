package li.cil.oc.server.fs

import dan200.computercraft.api.filesystem.IMount

class ComputerCraftFileSystem(val mount: IMount) extends InputStreamFileSystem {
  override def spaceTotal = 0

  override def spaceUsed = 0

  // ----------------------------------------------------------------------- //

  override def exists(path: String) = mount.exists(path)

  override def isDirectory(path: String) = mount.isDirectory(path)

	override def isRealDirectory(path: String) = mount.isDirectory(path)

  override def lastModified(path: String) = 0L

  override def list(path: String) = {
    val result = new java.util.ArrayList[String]
    mount.list(path, result)
    result.toArray.map(_.asInstanceOf[String])
  }

  override def size(path: String) = mount.getSize(path)

  // ----------------------------------------------------------------------- //

  protected def openInputChannel(path: String) = try {
    Some(new InputStreamChannel(mount.openForRead(path)))
  } catch {
    case _: Throwable => None
  }
}
