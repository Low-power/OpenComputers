package li.cil.oc.util

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.channels.Channels

import com.google.common.base.Strings
import li.cil.oc.OpenComputers
import li.cil.oc.Settings
import li.cil.oc.server.machine.Machine
import li.cil.oc.util.ExtendedLuaState._
import li.cil.repack.com.naef.jnlua
import li.cil.repack.com.naef.jnlua.LuaState
import li.cil.repack.com.naef.jnlua.NativeSupport.Loader
import org.apache.commons.lang3.SystemUtils

import scala.util.Random

/**
 * Factory singleton used to spawn new LuaState instances.
 *
 * This is realized as a singleton so that we only have to resolve shared
 * library references once during initialization and can then re-use the
 * already loaded ones.
 */
object LuaStateFactory {
  // ----------------------------------------------------------------------- //
  // Initialization
  // ----------------------------------------------------------------------- //

  /** Set to true in initialization code below if available. */
  private var haveNativeLibrary = false

  private var currentLib = ""

	private val libraryName = {
		if (!Strings.isNullOrEmpty(Settings.get.forceNativeLib)) Settings.get.forceNativeLib

		else if (SystemUtils.IS_OS_FREE_BSD && Architecture.IS_OS_X64) "native.amd64-freebsd.so"
		else if (SystemUtils.IS_OS_FREE_BSD && Architecture.IS_OS_X86) "native.i386-freebsd.so"

		else if (SystemUtils.IS_OS_LINUX && Architecture.IS_OS_ARM) "native.arm-linux-gnueabi.so"
		else if (SystemUtils.IS_OS_LINUX && Architecture.IS_OS_X64) "native.amd64-linux-gnu.so"
		else if (SystemUtils.IS_OS_LINUX && Architecture.IS_OS_X86) "native.i386-linux-gnu.so"

		else if(SystemUtils.IS_OS_SOLARIS && Architecture.IS_OS_X86) "native.i386-solaris.so"
		else if(SystemUtils.IS_OS_SOLARIS && Architecture.IS_OS_X64) "native.amd64-solaris.so"
		else if(SystemUtils.IS_OS_SOLARIS && Architecture.IS_OS_SPARC64) "native.sparc64-solaris.so"

		else if (SystemUtils.IS_OS_MAC && Architecture.IS_OS_X64) "native.amd64-apple-darwin.dylib"
		else if (SystemUtils.IS_OS_MAC && Architecture.IS_OS_X86) "native.i386-apple-darwin.dylib"

		//else if (SystemUtils.IS_OS_WINDOWS && Architecture.IS_OS_X64) "native.64.dll"
		//else if (SystemUtils.IS_OS_WINDOWS && Architecture.IS_OS_X86) "native.32.dll"

		else null
	}

  def isAvailable = haveNativeLibrary

  val is64Bit = Architecture.IS_OS_X64

  // Register a custom library loader with JNLua. We have to trigger
  // library loads through JNLua to ensure the LuaState class is the
  // one loading the library and not the other way around - the native
  // library also references the LuaState class, and if it is loaded
  // that way, it will fail to access native methods in its static
  // initializer, because the native lib will not have been completely
  // loaded at the time the initializer runs.
  jnlua.NativeSupport.getInstance().setLoader(new Loader {
    def load() {
      System.load(currentLib)
    }
  })

  // Since we use native libraries we have to do some work. This includes
  // figuring out what we're running on, so that we can load the proper shared
  // libraries compiled for that system. It also means we have to unpack the
  // shared libraries somewhere so that we can load them, because we cannot
  // load them directly from a JAR.
  def init() {
      OpenComputers.log.info(s"libraryName = '${libraryName}'")
    if (libraryName == null) {
      return
    }

    if (SystemUtils.IS_OS_WINDOWS && !Settings.get.alwaysTryNative) {
      if (SystemUtils.IS_OS_WINDOWS_XP) {
        OpenComputers.log.warn("Sorry, but Windows XP isn't supported. I'm afraid you'll have to use a newer Windows. I very much recommend upgrading your Windows, anyway, since Microsoft has stopped supporting Windows XP in April 2014.")
        return
      }

      if (SystemUtils.IS_OS_WINDOWS_2003) {
        OpenComputers.log.warn("Sorry, but Windows Server 2003 isn't supported. I'm afraid you'll have to use a newer Windows.")
        return
      }
    }

    val libraryUrl = classOf[Machine].getResource("/assets/" + Settings.resourceDomain + "/lib/" + libraryName)
    if (libraryUrl == null) {
      OpenComputers.log.warn(s"Native library with name '$libraryName' not found.")
      return
    }

    val tmpLibName = "OpenComputersMod-" + OpenComputers.Version + "-" + libraryName
    val tmpBasePath = if (Settings.get.nativeInTmpDir) {
      val path = System.getProperty("java.io.tmpdir")
      if (path == null) ""
      else if (path.endsWith("/") || path.endsWith("\\")) path
      else path + "/"
    }
    else ""
    val tmpLibFile = new File(tmpBasePath + tmpLibName)
    OpenComputers.log.info(s"temp file: '${tmpLibFile.getName}'.")

    // If the file, already exists, make sure it's the same we need, if it's
    // not disable use of the natives.
    if (tmpLibFile.exists()) {
      OpenComputers.log.info("temp file exists")
      var matching = true
      try {
        val inCurrent = libraryUrl.openStream()
        val inExisting = new FileInputStream(tmpLibFile)
        var inCurrentByte = 0
        var inExistingByte = 0
        do {
          inCurrentByte = inCurrent.read()
          inExistingByte = inExisting.read()
          if (inCurrentByte != inExistingByte) {
            matching = false
            inCurrentByte = -1
            inExistingByte = -1
          }
        }
        while (inCurrentByte != -1 && inExistingByte != -1)
        inCurrent.close()
        inExisting.close()
      }
      catch {
        case _: Throwable =>
          matching = false
      }
      if (!matching) {
        // Try to delete an old instance of the library, in case we have an update
        // and deleteOnExit fails (which it regularly does on Windows it seems).
        // Note that this should only ever be necessary for dev-builds, where the
        // version number didn't change (since the version number is part of the name).
        try {
          tmpLibFile.delete()
        }
        catch {
          case t: Throwable => // Ignore.
        }
        if (tmpLibFile.exists()) {
          OpenComputers.log.warn(s"Could not update native library '${tmpLibFile.getName}'!")
        }
      }
    }

    // Copy the file contents to the temporary file.
    try {
      OpenComputers.log.info("copying library to temp file")
      val in = Channels.newChannel(libraryUrl.openStream())
      try {
        val out = new FileOutputStream(tmpLibFile).getChannel
        try {
          out.transferFrom(in, 0, Long.MaxValue)
          tmpLibFile.deleteOnExit()
          // Set file permissions more liberally for multi-user+instance servers.
          tmpLibFile.setReadable(true, false)
          tmpLibFile.setWritable(true, false)
          tmpLibFile.setExecutable(true, false)
        }
        finally {
          out.close()
        }
      }
      finally {
        in.close()
      }
    }
    catch {
      // Java (or Windows?) locks the library file when opening it, so any
      // further tries to update it while another instance is still running
      // will fail. We still want to try each time, since the files may have
      // been updated.
      // Alternatively, the file could not be opened for reading/writing.
      //case t: Throwable => // Nothing.
      case t: Throwable =>
        OpenComputers.log.trace(s"Could not copy native library to '${tmpLibFile.getName}'.", t)
      OpenComputers.log.warn("exception on copy library")
    }
    // Try to load the lib.
    currentLib = tmpLibFile.getAbsolutePath
    OpenComputers.log.info(s"currentLib = $currentLib")
    try {
      new jnlua.LuaState().close()
      OpenComputers.log.info(s"Found a compatible native library: '${tmpLibFile.getName}'.")
      haveNativeLibrary = true
    }
    catch {
      case t: Throwable =>
        if (Settings.get.logFullLibLoadErrors) {
          OpenComputers.log.trace(s"Could not load native library '${tmpLibFile.getName}'.", t)
		OpenComputers.log.warn(t)
        }
        else {
          OpenComputers.log.trace(s"Could not load native library '${tmpLibFile.getName}'.")
        }
        tmpLibFile.delete()
      OpenComputers.log.warn("exception on load library")
    }
  }

  init()

  if (!haveNativeLibrary) {
    OpenComputers.log.warn("Unsupported platform, you won't be able to host games with persistent computers.")
    OpenComputers.log.info(s"libraryName = '$libraryName'")
  }

  // ----------------------------------------------------------------------- //
  // Factory
  // ----------------------------------------------------------------------- //

  def createState(): Option[LuaState] = {
    if (!haveNativeLibrary) return None

    try {
      val state =
        if (Settings.get.limitMemory) new jnlua.LuaState(Int.MaxValue)
        else new jnlua.LuaState()
      try {
        // Load all libraries.
        state.openLib(jnlua.LuaState.Library.BASE)
        state.openLib(jnlua.LuaState.Library.BIT32)
        state.openLib(jnlua.LuaState.Library.COROUTINE)
        state.openLib(jnlua.LuaState.Library.DEBUG)
        state.openLib(jnlua.LuaState.Library.ERIS)
        state.openLib(jnlua.LuaState.Library.MATH)
        state.openLib(jnlua.LuaState.Library.STRING)
        state.openLib(jnlua.LuaState.Library.TABLE)
        state.pop(8)

        if (!Settings.get.disableLocaleChanging) {
          state.openLib(jnlua.LuaState.Library.OS)
          state.getField(-1, "setlocale")
          state.pushString("C")
          state.call(1, 0)
          state.pop(1)
        }

        // Prepare table for os stuff.
        state.newTable()
        state.setGlobal("os")

        // Kill compat entries.
        state.pushNil()
        state.setGlobal("unpack")
        state.pushNil()
        state.setGlobal("loadstring")
        state.getGlobal("math")
        state.pushNil()
        state.setField(-2, "log10")
        state.pop(1)
        state.getGlobal("table")
        state.pushNil()
        state.setField(-2, "maxn")
        state.pop(1)

        // Remove some other functions we don't need and are dangerous.
        state.pushNil()
        state.setGlobal("dofile")
        state.pushNil()
        state.setGlobal("loadfile")

        state.getGlobal("math")

        // We give each Lua state it's own randomizer, since otherwise they'd
        // use the good old rand() from C. Which can be terrible, and isn't
        // necessarily thread-safe.
        val random = new Random
        state.pushScalaFunction(lua => {
          val r = random.nextDouble()
          lua.getTop match {
            case 0 => lua.pushNumber(r)
            case 1 =>
              val u = lua.checkNumber(1)
              lua.checkArg(1, 1 <= u, "interval is empty")
              lua.pushNumber(math.floor(r * u) + 1)
            case 2 =>
              val l = lua.checkNumber(1)
              val u = lua.checkNumber(2)
              lua.checkArg(2, l <= u, "interval is empty")
              lua.pushNumber(math.floor(r * (u - l + 1)) + l)
            case _ => throw new IllegalArgumentException("wrong number of arguments")
          }
          1
        })
        state.setField(-2, "random")

        state.pushScalaFunction(lua => {
          random.setSeed(lua.checkNumber(1).toLong)
          0
        })
        state.setField(-2, "randomseed")

        // Pop the math table.
        state.pop(1)

        return Some(state)
      }
      catch {
        case t: Throwable =>
          OpenComputers.log.warn("Failed creating Lua state.", t)
          state.close()
      }
    }
    catch {
      case _: UnsatisfiedLinkError =>
        OpenComputers.log.error("Failed loading the native libraries.")
      case t: Throwable =>
        OpenComputers.log.warn("Failed creating Lua state.", t)
    }
    None
  }

  // Inspired by org.apache.commons.lang3.SystemUtils
  object Architecture {
    val OS_ARCH = try System.getProperty("os.arch") catch {
      case ex: SecurityException => null
    }

    val IS_OS_ARM = isOSArchMatch("arm")

    val IS_OS_X86 = isOSArchMatch("x86") || isOSArchMatch("i386")

    val IS_OS_X64 = isOSArchMatch("x86_64") || isOSArchMatch("amd64")

		val IS_OS_SPARC64 = isOSArchMatch("sparcv9")

    private def isOSArchMatch(archPrefix: String): Boolean = OS_ARCH != null && OS_ARCH.startsWith(archPrefix)
  }

}
