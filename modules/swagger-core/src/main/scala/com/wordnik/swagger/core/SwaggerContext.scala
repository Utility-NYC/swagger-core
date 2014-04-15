package com.wordnik.swagger.core

import collection.mutable.ListBuffer
import org.slf4j.{ LoggerFactory, Logger }
import com.wordnik.swagger.core.util.ClassWrapper

object SwaggerContext {
  private val LOGGER = LoggerFactory.getLogger("com.wordnik.swagger.core.SwaggerContext")

  var suffixResponseFormat = true

  private val classLoaders = ListBuffer.empty[ClassLoader]
  registerClassLoader(this.getClass.getClassLoader)

  def registerClassLoader(cl: ClassLoader) = this.classLoaders += cl

  def loadClass(name: String): ClassWrapper = {
    var cls: ClassWrapper = null
    val itr = classLoaders.reverse.iterator
    while (cls == null && itr.hasNext) {
      try {
        val classLoader = itr.next
        cls = ClassWrapper.forName(name.trim, name => name match {
          case "List" => classOf[List[_]]
          case "Array" => classOf[Array[_]]
          case "Set" => classOf[Set[_]]
          case "Map" => classOf[Map[_,_]]
          case name => Class.forName(name, true, classLoader)
        })
      } catch {
        case e: ClassNotFoundException => {
          LOGGER.debug("Class %s not found in classLoader".format(name))
        }
      }
    }
    if (cls == null)
      throw new ClassNotFoundException("class " + name + " not found")
    cls
  }
}
