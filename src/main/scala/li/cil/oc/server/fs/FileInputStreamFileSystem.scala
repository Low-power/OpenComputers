package li.cil.oc.server.fs

import java.io

trait FileInputStreamFileSystem extends InputStreamFileSystem {
  protected def root: io.File

  // ----------------------------------------------------------------------- //

  override def spaceTotal = spaceUsed

  override def spaceUsed = spaceUsed_

  private lazy val spaceUsed_ = {
    def recurse(path: io.File): Long = {
      if (path.isDirectory)
        path.listFiles.foldLeft(0L)((acc, f) => acc + recurse(f))
      else
        path.length
    }
    recurse(root)
  }

  // ----------------------------------------------------------------------- //

  override def exists(path: String) = new io.File(root, path).exists()

  override def size(path: String) = new io.File(root, path) match {
    case file if file.isFile => file.length()
    case _ => 0L
  }

  override def isDirectory(path: String) = new io.File(root, path).isDirectory

	override def isRealDirectory(path: String) = {
		//li.cil.oc.OpenComputers.log.info("methon: isRealDirectory(" + path + ")")
		var file = new io.File(root, path)
		var abpath = file.getAbsolutePath()
		//li.cil.oc.OpenComputers.log.info("isDirectory ? " + file.isDirectory())
		//li.cil.oc.OpenComputers.log.info("AbsolutePath: " + abpath)
		//li.cil.oc.OpenComputers.log.info("CanonicalPath: " + file.getCanonicalPath())
		abpath = abpath.replace('\\', '/')
		if(abpath.charAt(abpath.length() - 1) == '/') abpath = abpath.substring(0, abpath.length() - 1)
		//li.cil.oc.OpenComputers.log.info("NewAbsolutePath: " + abpath)
		file.isDirectory() && abpath.equals(file.getCanonicalPath().replace('\\', '/'))
	}

	override def lastModified(path: String) = new io.File(root, path).lastModified

	override def list(path: String) = new io.File(root, path) match {
		case file if file.exists() && file.isFile => Array(file.getName)
		case directory if directory.exists() && directory.isDirectory && directory.list() != null =>
			if(path.length() > 512) throw new io.IOException("path too long " + path)
			directory.listFiles().map(file => if (file.isDirectory) file.getName + "/" else file.getName)
		case _ => new Array[String](0)
	}

  // ----------------------------------------------------------------------- //

  override protected def openInputChannel(path: String) = Some(new FileChannel(new io.File(root, path)))

  protected class FileChannel(file: io.File) extends InputChannel {
    val channel = new io.RandomAccessFile(file, "r").getChannel

    override def position(newPosition: Long) = {
      channel.position(newPosition)
      channel.position
    }

    override def position = channel.position

    override def close() = channel.close()

    override def isOpen = channel.isOpen

    override def read(dst: Array[Byte]) = channel.read(java.nio.ByteBuffer.wrap(dst))
  }

}
