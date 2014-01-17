package scalac

import java.io._
import java.net._
import java.lang.{Runtime, System, Thread}
import scala.collection.mutable.{ListBuffer,HashMap}
import scala.concurrent.ops._
import org.gjt.sp.jedit._
import org.gjt.sp.util.Log
import org.gjt.sp.jedit.gui.DockableWindowManager
import org.gjt.sp.jedit.EditPlugin
import org.gjt.sp.util.IOUtilities
import org.gjt.sp.jedit.MiscUtilities._

import org.mortbay.jetty.Connector
import org.mortbay.jetty.Server
import org.mortbay.jetty.nio.SelectChannelConnector
import org.mortbay.jetty.webapp.WebAppContext

object jetty {

    var scanInterval = 2
    var server: Server = null
    val cache = new HashMap[String,Long]
    val classpath = new ListBuffer[String]
        
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
    
    // Start Web server on port and root with additional classes directory that
    // will be monitored for changed
    def start(port: Int, warPath: String, classesPath: String, jarPath: String) = {
        
        if (server != null) {
            stop
        }
        scalac.compiler.prepare
        scalac.compiler.printf("jetty -port " + port + " -war " + warPath + " -classes " + classesPath + " -jar " + jarPath)  
        
        // Actual server thread
        server = new Server()
        
        val connector = new SelectChannelConnector()
        connector.setPort(port)
        server.addConnector(connector)
        
        val webapp = new WebAppContext()
        webapp.setContextPath("/")
        webapp.setWar(warPath)
        
        // Build new classpath
        classpath.clear
        
        // Our classes directory, make sure we have trailing slash
        classpath += classesPath + File.separator
        
        // Add all .jar files
        for (f <- jarPath.split(File.pathSeparator)) {
            scalac.compiler.findFiles(new File(f), scalac.compiler.jarExt).foreach(classpath += _)
        }
        
        // Add system classpath and system .jars
        scalac.compiler.systemClasspath(classpath)
        
        // Rebuild classpath with full urls
        val paths = classpath.mkString(File.pathSeparator).split(File.pathSeparator)
        val urls = new Array[URL](paths.length)
        var i = 0
        
        for (p <- paths) {
            urls(i) = new URL((new File(p)).
            
            toURI().toString) 
            i += 1
        }
        
        // Jetty requires ; or , to be separators, does not use File.pathSeparator
        webapp.setExtraClasspath(paths.mkString(";"))
        server.setHandler(webapp)
        
        spawn { 
            // Make extra classes available to the server as well
            var parent = Thread.currentThread().getContextClassLoader()
            
            if (parent == null) {
                parent = this.getClass().getClassLoader()
            }
            if (parent == null) {
                parent = ClassLoader.getSystemClassLoader()
            }
        
            val loader = new URLClassLoader(urls, parent)
            Thread.currentThread().setContextClassLoader(loader)
        
            server.start()
            
            // Put all classes into cache
            cache.clear
            scalac.compiler.findFiles(new File(classesPath), scalac.compiler.classExt).foreach(isModified)

            scalac.compiler.printf("jetty started")
            
            while (server.isStopped() == false && server.isFailed() == false) {
                Thread.sleep(scanInterval * 1000)
                
                if (scalac.compiler.findFiles(new File(classesPath), scalac.compiler.classExt).toList.exists(isModified)) {
                    
                    scalac.compiler.printf("jetty reloading...")
                    
                    try {
                        webapp.stop()
                    } catch {
                        case e: Exception => Log.log(Log.ERROR, this, e.printStackTrace())
                    }
                    
                    try {
                        webapp.start()
                    } catch {
                        case e: Exception => Log.log(Log.ERROR, this, e.printStackTrace())
                    }
                    
                    // Reload files cache
                    cache.clear
                    scalac.compiler.findFiles(new File(classesPath), scalac.compiler.classExt).foreach(isModified)
                }
            }
            scalac.compiler.printf("jetty stopped")
            server = null
        }
    }

    def stop = {
        if (server != null) {
            server.stop()
        }
    }
}
