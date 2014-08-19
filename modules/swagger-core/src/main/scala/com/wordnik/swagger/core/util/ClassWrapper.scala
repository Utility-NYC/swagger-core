package com.wordnik.swagger.core.util

import java.lang.reflect._
import scala.collection.mutable.{ListBuffer, HashMap}
import java.lang.annotation.Annotation
import java.util.IdentityHashMap
import scala.collection.convert.Wrappers.JMapWrapper
import scala.collection.mutable
import sun.reflect.generics.reflectiveObjects.{GenericArrayTypeImpl, ParameterizedTypeImpl}
import scala.util.parsing.combinator.JavaTokenParsers

object ClassWrapper {
  def apply(`type`: Type) = new ClassWrapper(`type`)
  def forName(name: String): ClassWrapper = forName(name, name => Class.forName(name))
  def forName(name: String, classLoader: String => Class[_]): ClassWrapper = {
    apply(new NameParser(classLoader).apply(name))
  }

  implicit def classToClassWrapper(cls: Class[_]) = ClassWrapper(cls)

  private class NameParser(classLoader: String => Class[_]) extends JavaTokenParsers {
    def `class`: Parser[Class[_]] = ident ~ rep("." ~> ident) ^^ {
      case x ~ y => classLoader((x /: y)(_ + "." + _))
    }

    def typeList: Parser[scala.Array[Type]] = "[" ~> `type` ~ rep("," ~> `type`) <~ "]" ^^ {
      case x ~ y => ((ListBuffer(x) /: y)(_ += _)).toArray
    }

    def `type`: Parser[Type] = array | (`class` ~ (typeList?)) ^^ {
      case x ~ None => x
      case x ~ Some(y) => ParameterizedTypeImpl.make(x, y.toArray, null)
    }

    def array: Parser[Type] = "array[" ~> `type` <~ "]" ^^ {
      case x: Class[_] => classLoader("[L" + x.getName + ";")
      case x: ParameterizedType => GenericArrayTypeImpl.make(x)
    }

    def apply(input: String): Type = parseAll(`type`, input) match {
      case Success(result, _) => result
      case _: NoSuccess => null
    }
  }
}

class ClassWrapper(private val `type`: Type, private val outerScope: ClassWrapper) {

  private var clazz: Class[_] = classOf[Object]
  private var typeMap: Map[String, ClassWrapper] = Map()
  private var superclass: ClassWrapper = _
  private var arrayComponent: ClassWrapper = _
  private var parsedClasses: mutable.Map[Type, ClassWrapper] = {
    if (outerScope == null) {
      JMapWrapper(new IdentityHashMap[Type, ClassWrapper]())
    } else {
      outerScope.parsedClasses
    }
  }

  parseType(`type`)

  if (clazz.getGenericSuperclass != null) {
    superclass = getOrCreateClassWrapper(clazz.getGenericSuperclass, this)
  }

  def this(`type`: Type) = this(`type`, null)

  def getName: String = {
    val clazzName = {
      if (isArray) {
        "array[" + arrayComponent.getName + "]"
      } else {
        clazz.getName
      }
    }
    getName(clazzName)
  }

  def getName(clazzName: String): String = {
    val types = clazz.getTypeParameters.map(t => getTypeArgument(t.getName).getName)
    clazzName + (if (types.length > 0) types.mkString("[", ",", "]") else "")
  }

  def getSimpleName: String = {
    val clazzName = {
      if (isArray) {
        "array[" + arrayComponent.getSimpleName + "]"
      } else {
        clazz.getSimpleName
      }
    }
    getSimpleName(clazzName)
  }

  def getSimpleName(clazzName: String): String = {
    val types = clazz.getTypeParameters.map(t => getTypeArgument(t.getName).getSimpleName)
    clazzName + (if (types.length > 0) types.mkString("[", ",", "]") else "")
  }

  def getSuperclass: ClassWrapper = superclass

  def getRawClass: Class[_] = clazz

  def getRawType: Type = `type`

  def getTypeArgument(arg: String): ClassWrapper = {
    typeMap.get(arg) match {
      case Some(t) => t
      case None => {
        if (outerScope != null) {
          outerScope.getTypeArgument(arg)
        } else {
          null
        }
      }
    }
  }

  def isArray: Boolean = arrayComponent != null

  def getArrayComponent: ClassWrapper = arrayComponent

  def getMethodReturnType(name: String, parameterTypes: Class[_]*): ClassWrapper = {
    val method = clazz.getMethod(name, parameterTypes: _*)
    getMethodReturnType(method)
  }

  def getMethodReturnType(method: Method): ClassWrapper = {
    val methodClass = method.getDeclaringClass

    var genericClass = this
    while (genericClass != null && genericClass.clazz != methodClass) {
      genericClass = genericClass.superclass
    }
    assert(genericClass != null)

    getOrCreateClassWrapper(method.getGenericReturnType, genericClass)
  }

  def getFieldType(name: String): ClassWrapper = {
    val field = clazz.getField(name)
    getFieldType(field)
  }

  def getFieldType(field: Field): ClassWrapper = {
    val fieldClass = field.getDeclaringClass

    var genericClass = this
    while (genericClass != null && genericClass.clazz != fieldClass) {
      genericClass = genericClass.superclass
    }
    assert(genericClass != null)

    getOrCreateClassWrapper(field.getGenericType, genericClass)
  }

  // forward to clazz
  def newInstance = clazz.newInstance
  def getAnnotation[T <: Annotation](cls: Class[T]): T = clazz.getAnnotation(cls)
  def getAnnotations: Seq[Annotation] = clazz.getAnnotations()
  def isEnum: Boolean = clazz.isEnum
  def getEnumConstants: Seq[_] = clazz.getEnumConstants
  def getDeclaredMethods: Seq[Method] = clazz.getDeclaredMethods
  def getDeclaredFields: Seq[Field] = clazz.getDeclaredFields
  def getDeclaredField(name: String): Field = clazz.getDeclaredField(name)

  override def toString: String = getName

  // private methods
  private def parseType(`type`: Type) = {
    assert(!parsedClasses.contains(`type`))
    parsedClasses += `type` -> this
    `type` match {
      case t: Class[_] => parseClass(t)
      case t: ParameterizedType => parseParameterizedType(t)
      case t: GenericArrayType => parseGenericArrayType(t)
      case t: TypeVariable[_] => parseTypeVariable(t)
      case t: WildcardType => ()
      case _ => ()
    }
  }

  private def parseClass(`type`: Class[_]) = {
    clazz = `type`
    if (clazz.isArray) {
      arrayComponent = getOrCreateClassWrapper(clazz.getComponentType)
    }
    if (clazz.getTypeParameters.size > 0) {
      typeMap = clazz.getTypeParameters.map(t => t.getName -> getOrCreateClassWrapper(classOf[Object])).toMap
    }
  }

  private def parseParameterizedType(`type`: ParameterizedType) = {
    val declaredTypes = `type`.getRawType.asInstanceOf[Class[_]].getTypeParameters
    val actualTypes = `type`.getActualTypeArguments
    assert(declaredTypes.length == actualTypes.length)

    val results = new HashMap[String, ClassWrapper]
    for (i <- 0 until declaredTypes.length) {
      results += (declaredTypes(i).getName -> getOrCreateClassWrapper(actualTypes(i), this))
    }

    clazz = `type`.getRawType.asInstanceOf[Class[_]]
    typeMap = results.toMap
  }

  private def parseGenericArrayType(`type`: GenericArrayType) = {
    val componentClass = getOrCreateClassWrapper(`type`.getGenericComponentType, this)
    val className: String = "[L" + componentClass.getRawClass.getName + ";"

    clazz = Class.forName(className)
    arrayComponent = componentClass
  }

  private def parseTypeVariable(`type`: TypeVariable[_]) = {
    val genericDeclaration: GenericDeclaration = `type`.getGenericDeclaration.asInstanceOf[GenericDeclaration]
    if (genericDeclaration.isInstanceOf[Class[_]]) {
      val t: ClassWrapper = outerScope.getTypeArgument(`type`.getName)
      if (t != null) {
        clazz = t.clazz
        typeMap = t.typeMap
      }
    }
  }

  private def getClassName() = {
    if (isArray) {
      "array[" + arrayComponent.getName + "]"
    } else {
      clazz.getName
    }
  }

  private def getClassSimpleName() = {
    if (isArray) {
      "array[" + arrayComponent.getSimpleName + "]"
    } else {
      clazz.getSimpleName
    }
  }

  private def getOrCreateClassWrapper(`type`: Type, parent: ClassWrapper = null): ClassWrapper = {
    parsedClasses.get(`type`) match {
      case Some(t: ClassWrapper) => t
      case None => new ClassWrapper(`type`, parent)
    }
  }

}
