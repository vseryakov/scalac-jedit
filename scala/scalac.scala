package scalac

import java.io._
import java.util.zip._
import java.lang.{Runtime, System, Thread}
import scala.collection.mutable.{ListBuffer,HashMap}
import scala.tools.nsc.Settings
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Global
import scala.tools.nsc.FatalError
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.FakePos
import scala.tools.nsc.CompilerCommand
import org.gjt.sp.jedit._
import org.gjt.sp.util.Log
import org.gjt.sp.jedit.gui.DockableWindowManager
import org.gjt.sp.jedit.EditPlugin
import org.gjt.sp.util.IOUtilities
import org.gjt.sp.jedit.MiscUtilities._
import console.{Console,Output,CommandOutputParser}
import errorlist.ErrorList
import javax.swing.{JTextField,JTextArea}
import org.gjt.sp.jedit.gui._
import java.awt.GridBagConstraints

object compiler {

    val version = "1.1.0"
    val jarExt = ".+\\.[jJ][aA][rR]$"
    val scalaExt = ".+\\.[sS][cC][aA][lL][aA]$"
    val classExt = ".+\\.[cC][lL][aA][sS][sS]$"

    var view: View = null
    var console: Console = null
    var errorlist: ErrorList = null
    var compiler: Global = null
    var reporter: ConsoleReporter = null
    var dock: DockableWindowManager = null
    var parser: CommandOutputParser = null
    val classpath = new ListBuffer[String]
    val cache = new HashMap[String,Long]
    val runtime = Runtime.getRuntime()
   
    // Setup environment, activate console window
    def prepare: Boolean = {
        if (view == null) {
            view = jEdit.getActiveView()
        }
    
        if (dock == null) {
            dock = view.getDockableWindowManager() 
        }
    
        // Make sure we have console plugin active
        if (console == null) {
            console = dock.getDockable("console").asInstanceOf[Console]
            if (console == null) {
                dock.addDockableWindow("console")
                console = dock.getDockable("console").asInstanceOf[Console]
                if (console == null) {
                    printf("fatal: Console plugin not found")
                    return false               
                }
            }
        }
        
        // Make sure we have errorlist plugin active
        if (errorlist == null) {
            errorlist = dock.getDockable("error-list").asInstanceOf[ErrorList]
            if (errorlist == null) {
                dock.addDockableWindow("error-list")
                errorlist = dock.getDockable("error-list").asInstanceOf[ErrorList]
                if (errorlist == null) {
                    printf("fatal: ErroList plugin not found")
                    return false               
                }
            }
        }
        
        if (parser == null) {
            parser = new CommandOutputParser(console.getView(), console.getErrorSource(), console.getPlainColor());
        }
        
        parser.setDirectory(view.getBuffer().getDirectory())
        console.getErrorSource().clear()
        console.setShell("System")
        console.clear()
        view.getDockableWindowManager().showDockableWindow("console")
        return true
    }
  
    def error(msg: String) = {
        printf(msg)
    }
   
    // Return all files that match given regex, recursevily
    def findFiles(file: File, ext: String): ListBuffer[String] = {
        var files = new ListBuffer[String]
        if (file != null) {
            if (file.isFile() && file.getAbsolutePath().matches(ext)) {
                files += file.getAbsolutePath()
            } else
            if (file.isDirectory()) {
                for (f <- file.listFiles()) {
                    findFiles(f, ext).foreach(files += _)
                }
            }
        }
        files
    }

    // Return true if this source file does not need to be compiled, i.e. it is not modified or does not exist
    def isModified(file: String): Boolean = {
        val fd = new File(file)
        val path = fd.getAbsolutePath
        if (fd.exists()) {
            if (!cache.contains(path) || cache(path) < fd.lastModified) {
                cache(path) = fd.lastModified
                return true
            }
        }
        return false
    }

    // Compare 2 settings objects until native implementation fixed
    def isSame(s1: Settings, s2: Settings): Boolean = {
        for (i <- 1 until s1.allSettings.length) { 
            val a = s1.allSettings(i)
            val b = s2.allSettings(i)
            if (a.unparse != b.unparse) {
                return false
            }
        }
        return true
    }
    
    // Flush file from cache
    def cacheFlush(file: String) = {
        if (cache.contains(file)) {
            cache -= file;
        }
    }

    // Output text to to the console
    def printf(msg: String): Unit = {
        console.getOutput().print(null, msg);
    }
    
    // Add all .jar files from system classpath
    def systemClasspath(path: ListBuffer[String]) = {
        val jars = jEdit.getProperty("scalac.jarpath")
        if (jars != null) {
           for (f <- jars.split(File.pathSeparator)) {
               findFiles(new File(f), jarExt).foreach(path += _)
           }
        }
         
        // Add system classpath as it is
        val classes = jEdit.getProperty("scalac.classpath")
        if (classes != null) {
            path += classes
        }
    }

    // Copy file from local resources into destination file
    def copyFile(src: String, dst: String): Unit = {
        Log.log(Log.NOTICE, this, "copying file " + src + " to " + dst)

        try {
            val ifile = new File(jEdit.getSettingsDirectory(), "scalac" + File.separator + src)
            val ofile = new File(dst)
            val odir = new File(ofile.getParent)
            odir.mkdirs

            var in: FileInputStream = null
            var out: FileOutputStream = null

            try {
                in = new FileInputStream(ifile)
                out = new FileOutputStream(ofile)
                IOUtilities.copyStream(null, in, out, false)
            } catch {
                case e: Exception => Log.log(Log.ERROR, this, e.printStackTrace())
            }

            IOUtilities.closeQuietly(in)
            IOUtilities.closeQuietly(out)            
        } catch {
            case e: Exception => Log.log(Log.ERROR, this, e.printStackTrace())
        }
    }

    // Delete file from home directory
    def deleteFile(file: String): Unit = {
        Log.log(Log.NOTICE, this, "deleting file " + file)

        try {
            new File(constructPath(jEdit.getSettingsDirectory(), file)).delete
        } catch {
            case e: Exception => Log.log(Log.ERROR, this, e.printStackTrace())
        }
    }

    // Create new application structure
    def create(dir: String): Unit = {
        copyFile("web.xml", dir + File.separator + "webapp" + File.separator + "WEB-INF" + File.separator + "web.xml")
        copyFile("index.html", dir + File.separator + "webapp" + File.separator + "index.html")
        copyFile("default.html", dir + File.separator + "webapp" + File.separator + "default.html")
        copyFile("boot.scala", dir + File.separator + "scala" + File.separator + "boot.scala")
    }

    // Run compiler with arguments as plain string
    def run(file: String): Unit = {
        run(file.split(" "))
    }
  
    // Run compiler with arguments in array
    def run(args: Array[String]): Unit = {
        if (!prepare) {
            return    
        }
        printf("scalac" + args.mkString(" "))
        
        val start_time = compat.Platform.currentTime
        var params = new ListBuffer[String]
        var files = new ListBuffer[String]
        val settings = new Settings(error)
        var scala_flag = false
        var class_flag = false
        var dest_flag = false
        var jar_flag = false
        val ss = new Settings(error)

        classpath.clear
        
        // Add all .scala files from the project directory
        for (p <- args) {
            if (p == "-d") {
                dest_flag = true
                params += p
            } else
            if (p == "-classpath") {
                class_flag = true
            } else
            if (p == "-scalapath") {
                scala_flag = true
            } else
            if (p == "-jarpath") {
                jar_flag = true
            } else
            if (dest_flag) {
                // Make sure destination directory in the classpath
                dest_flag = false
                classpath += File.pathSeparator + p + File.separator
                params += p
            } else
            if (class_flag) {
                // Add to classpath as it is given
                class_flag = false
                classpath += File.pathSeparator + p
            } else
            if (scala_flag) {
                // All source files recursively
                for (f <- p.split(File.pathSeparator)) {
                    findFiles(new File(f), scalaExt).foreach(files += _)
                }
                scala_flag = false
            } else
            if (jar_flag) {
                // All jar files recusively
                for (f <- p.split(File.pathSeparator)) {
                    findFiles(new File(f), jarExt).foreach(classpath += _)
                }
                jar_flag = false
            } else {
                // All other arguments ara added as they are
                params += p
            }
        }
        // Append all project source files at the end of the list
        files.foreach(params += _)
        
        // Parse command line parameters
        val command = new CompilerCommand(params.toList, settings, error, false)

        // Add system classes
        systemClasspath(classpath)
    
        // Append Scala plugin jar because compiler and library must be in the classpath
        classpath += constructPath(jEdit.getSettingsDirectory(), "jars", this.getClass.getClassLoader.toString.split(" ")(0))
        
        // Build final class path
        settings.classpath.value = classpath.mkString(File.pathSeparator)
      
        // Remove files that are not modified since the last compilation
        val sources = command.files.filter(isModified)

        // Output debugging info before invoking the compiler to know what files and jars were discovered
        if (command.settings.verbose.value) {
            printf("FILES=" + command.files)
            printf("MODIFIED=" + sources)
            printf("CLASSPATH=" + command.settings.classpath.value)
            printf("Settings=" + command.settings.allSettings.length)
            for (s <- command.settings.allSettings) {
                if (s.unparse.length > 0) printf(s.unparse.toString)
            }
        }

        if (sources.length == 0) {
            printf("Nothing to compile")
            return
        }

        // Redirect reporter to our console
        reporter = new ConsoleReporter(command.settings) {
            override def displayPrompt = ()
        
            override def printMessage(msg: String) = { 
               console.getOutput().print(null, msg)
               parser.processLine(msg);
            }
        
            override def flush = {
               parser.finishErrorParsing()
            }
        }
    
        try {
            // Reuse existing compiler if settings are the same
            if (compiler != null && isSame(command.settings, compiler.settings)) {
                compiler.settings = command.settings
                compiler.reporter = reporter
            } else {
                compiler = new Global(command.settings, reporter)
                printf("new compiler created")
            }
            
            val c = compiler
            val run = new c.Run
            run compile sources
            reporter.printSummary()
         
            // Check for errors and flush files with errors from cache
            val errors = console.getErrorSource().getAllErrors()
            if (errors != null) {
                for (e <- errors) {
                  cacheFlush(e.getFilePath())
                }
                view.getDockableWindowManager().showDockableWindow("error-list")
            }
            
            // Garbage collection after each compilation
            runtime.gc()
            if ((runtime.totalMemory() - runtime.freeMemory()).toDouble / runtime.maxMemory().toDouble > 0.8) {
                compiler = null
                printf("compiler destroyed, total=" + runtime.totalMemory() + " free=" +runtime.freeMemory() + " max=" + runtime.maxMemory())
            }

            // Activate error list window and show system console with messages               
            console.setShell("System")
            printf("done in " + (compat.Platform.currentTime - start_time) + " ms")
            
        } catch {
            case ex @ FatalError(msg) => reporter.error(null, "fatal error: " + msg)
        }
        return
  }
} 

class ScalacPlugin extends EditPlugin {

    override def start = {
        if (jEdit.getProperty("scalac.init") != compiler.version) {
            
            val file = new File(constructPath(jEdit.getSettingsDirectory(), "scalac"))
            if (!file.exists) {
                file.mkdirs
            }
            
            // On first init, copy resource files into settings directory
            copyResource("/resources/catalog", "modes" + File.separator + "catalog")
            copyResource("/resources/scala.xml", "modes" + File.separator + "scala.xml")
            copyResource("/resources/build.xml", "console" + File.separator + "commando" + File.separator + "build.xml")
            copyResource("/resources/maven.xml", "console" + File.separator + "commando" + File.separator + "maven.xml")
            copyResource("/resources/jetty.xml", "console" + File.separator + "commando" + File.separator + "jetty.xml")
            copyResource("/resources/scalac-compile.xml", "console" + File.separator + "commando" + File.separator + "scalac-compile.xml")
            copyResource("/resources/scalac-create.xml", "console" + File.separator + "commando" + File.separator + "scalac-create.xml")

            copyResource("/resources/web.xml", "scalac" + File.separator + "web.xml")
            copyResource("/resources/index.html", "scalac" + File.separator + "index.html")
            copyResource("/resources/default.html", "scalac" + File.separator + "default.html")
            copyResource("/resources/boot.scala", "scalac" + File.separator + "boot.scala")

            // Delete obsolete files
            compiler.deleteFile("console" + File.separator + "commando" + File.separator + "scalac.xml")

            // Bind compile command of the console to scala .bsh script
            jEdit.setProperty("mode.scala.commando.compile", "scalac-compile")
            jEdit.setProperty("scalac.jarpath", "/usr/local/scala")
            jEdit.setProperty("scalac.init", compiler.version)

            // Tell jedit and console to refresh their caches
            jEdit.reloadModes()
            console.ConsolePlugin.rescanCommands()
        }
    }

    // Copy file from resources into destination file
    def copyResource(src: String, dst: String): Unit = {
        Log.log(Log.NOTICE, this, "copying resource " + src + " to " + dst)

        try {
            val in = this.getClass().getResourceAsStream(src)
            val out = new FileOutputStream(constructPath(jEdit.getSettingsDirectory(), dst))

            IOUtilities.copyStream(null, in, out, false)
            IOUtilities.closeQuietly(in)
            IOUtilities.closeQuietly(out)            
        } catch {
            case e: Exception => Log.log(Log.ERROR, this, e.printStackTrace())
        }
    }
    
    override def stop = {
    }
}

class ScalacPluginOptionPane extends AbstractOptionPane("scalac") {

    private var classpath: JTextField = null
    private var jarpath: JTextField = null

    override def _init = {
       classpath = new JTextField(jEdit.getProperty("scalac.classpath"))
       addComponent("Class Path:", classpath)

       jarpath = new JTextField(jEdit.getProperty("scalac.jarpath"))
       addComponent("JAR Path:", jarpath)

       val text = new JTextArea()
       text.setLineWrap(true)
       text.setWrapStyleWord(true)
       text.setOpaque(false)
       text.setEditable(false)
       text.append("NOTE: classpath will be used as it is, jar path can have multiple directories in the same format as classapth and every directory will be scanned for .jar files and all .jar files will be added to Scala compiler classpath")
       addComponent(text, GridBagConstraints.HORIZONTAL)
    }

    override def _save = {
       jEdit.setProperty("scalac.classpath", classpath.getText())
       jEdit.setProperty("scalac.jarpath", jarpath.getText())
    }
}

