/**
 *  Copyright 2013 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.core.util

import com.wordnik.swagger.model._
import com.wordnik.swagger.converter.ModelConverters
import com.wordnik.swagger.core.{ SwaggerContext, SwaggerSpec, SwaggerTypes }

import org.slf4j.LoggerFactory

import scala.collection.mutable.{ ListBuffer, HashMap, HashSet }
import com.wordnik.swagger.reader.ModelReaders
import javax.xml.bind.annotation.{XmlEnum, XmlRootElement}

object ModelUtil {
  private val LOGGER = LoggerFactory.getLogger(ModelUtil.getClass)
  val ContainerMatcher = "(List|Array|Set)\\[(.*)\\].*?".r

  def stripPackages(apis: List[ApiDescription]): List[ApiDescription] = {
    (for(api <- apis) yield {
      val operations = (for(op <- api.operations) yield {
        val parameters = (for(param <- op.parameters) yield {
          param.copy(dataType = cleanDataType(param.dataType))
        }).toList
        val messages = (for(message <- op.responseMessages) yield {
          if(message.responseModel != None) {
            message.copy(responseModel = Some(cleanDataType(message.responseModel.get)))
          }
          else message
        }).toList
        op.copy(
          responseClass = cleanDataType(op.responseClass),
          parameters = parameters,
          responseMessages = messages)
      }).toList
      api.copy(operations = operations)
    }).toList
  }

  def cleanDataType(dataType: String) = {
    val typeInfo = dataType match {
      case ContainerMatcher(container, inner) => {
        val p = if(inner.indexOf(",") > 0)
          inner.split("\\,").last.trim
        else inner
        (Some(container), p)
      }
      case _ => (None, dataType)
    }
    val baseType = if(typeInfo._2.startsWith("java.lang")) {
      val trimmed = typeInfo._2.substring("java.lang".length + 1)
      if(SwaggerSpec.baseTypes.contains(trimmed.toLowerCase))
        trimmed.toLowerCase
      else
        trimmed
    }
    else {
      modelFromString(typeInfo._2) match {
        case Some(e) => e._1
        case None => typeInfo._2
      }
    }
    val normalized = SwaggerTypes(baseType) match {
      case "object" => baseType
      case e: String => e
    }
    // put back in container
    typeInfo._1 match {
      case Some(e) => "%s[%s]".format(e, normalized)
      case None => normalized
    }
  }

  def modelsFromApis(apis: List[ApiDescription]): Option[Map[String, Model]] = {
    val modelnames = new HashSet[String]()
    for(api <- apis; op <- api.operations) {
      modelnames ++= op.responseMessages.map{_.responseModel}.flatten.toSet
      modelnames += op.responseClass
      op.parameters.foreach(param => {
        LOGGER.debug("adding dependent model " + param.dataType)
        modelnames += param.dataType
      })
    }
    val models = (for(name <- modelnames) yield modelAndDependencies(name)).flatten.toMap
    if(models.size > 0) Some(models)
    else None
  }

  def modelAndDependencies(name: String): Map[String, Model] = {
    val typeRef = name match {
      case ContainerMatcher(containerType, basePart) => {
        LOGGER.debug("loading " + basePart + ", " + containerType)
        if(basePart.indexOf(",") > 0) // handle maps, i.e. List[String,String]
          basePart.split("\\,").last.trim
        else basePart
      }
      case _ => name
    }
    if(shoudIncludeModel(typeRef)) {
      ModelReaders.reader.read(typeRef) match {
        case None => {
          try {
            val cls = SwaggerContext.loadClass(typeRef)
            (for(model <- ModelConverters.readAll(cls)) yield (model.name, model)).toMap
          }
          catch {
            case e: ClassNotFoundException => Map()
          }
        }
        case Some(models) => models
      }
    }
    else Map()
  }

  def modelFromString(name: String): Option[Tuple2[String, Model]] = {
    val typeRef = name match {
      case ContainerMatcher(containerType, basePart) => basePart
      case _ => name
    }
    if(shoudIncludeModel(typeRef)) {
      try{
        val cls = SwaggerContext.loadClass(typeRef)
        if (!cls.isEnum) {
          ModelConverters.read(cls, ModelConverters.typeMap) match {
            case Some(model) => Some((toName(cls), model))
            case None => None
          }
        }
        else None
      }
      catch {
        case e: ClassNotFoundException => None
      }
    }
    else None
  }

  def toName(cls: ClassWrapper): String = {
    getDataType(cls, true)
  }

  def getDataType(returnType: ClassWrapper, isSimple: Boolean = false): String = {
    if (TypeUtil.isParameterizedList(returnType.getRawType)) {
      val typeParameters = returnType.getRawClass.getTypeParameters
      val types = typeParameters.map(t => getDataType(returnType.getTypeArgument(t.getName), isSimple))
      "List" + types.mkString("[", ",", "]")
    } else if (TypeUtil.isParameterizedSet(returnType.getRawType)) {
      val typeParameters = returnType.getRawClass.getTypeParameters
      val types = typeParameters.map(t => getDataType(returnType.getTypeArgument(t.getName), isSimple))
      "Set" + types.mkString("[", ",", "]")
    } else if (TypeUtil.isParameterizedMap(returnType.getRawType)) {
      val typeParameters = returnType.getRawClass.getTypeParameters
      val types = typeParameters.map(t => getDataType(returnType.getTypeArgument(t.getName), isSimple))
      "Map" + types.mkString("[", ",", "]")
    } else if (returnType.isArray) {
      var arrayClass = returnType.getArrayComponent
      "Array[" + getDataType(arrayClass, isSimple) + "]"
    } else if (returnType.getRawClass == classOf[Option[_]]) {
      val valueType = returnType.getTypeArgument(returnType.getRawClass.getTypeParameters.head.getName)
      getDataType(valueType, isSimple)
    } else if (classOf[Class[_]].isAssignableFrom(returnType.getRawClass)) {
      // ignore Class
      null
    } else {
      val typeParameters = returnType.getRawClass.getTypeParameters
      val types = typeParameters.map(t => getDataType(returnType.getTypeArgument(t.getName), isSimple))
      readName(returnType.getRawClass, isSimple) + {
        if (types.length > 0) types.mkString("[", ",", "]") else ""
      }
    }
  }

  def readName(hostClass: Class[_], isSimple: Boolean = true): String = {
    val xmlRootElement = hostClass.getAnnotation(classOf[XmlRootElement])
    val xmlEnum = hostClass.getAnnotation(classOf[XmlEnum])

    val name = {
      if (xmlEnum != null && xmlEnum.value() != null) {
        if (isSimple) readName(xmlEnum.value())
        else hostClass.getName
      } else if (xmlRootElement != null) {
        if ("##default".equals(xmlRootElement.name())) {
          if (isSimple) hostClass.getSimpleName
          else hostClass.getName
        } else {
          if (isSimple) readString(xmlRootElement.name())
          else hostClass.getName
        }
      } else if (hostClass.getName.startsWith("java.lang.") && isSimple) {
        hostClass.getName.substring("java.lang.".length)
      } else {
        if (isSimple) hostClass.getSimpleName
        else hostClass.getName
      }
    }
    validateDatatype(name)
  }

  def readString(s: String, existingValue: String = null, ignoreValue: String = null): String = {
    /*
    if (s == null && existingValue != null && existingValue.trim.length > 0) existingValue
    else if (s == null) null
    else if (s.trim.length == 0) null
    else if (ignoreValue != null && s.equals(ignoreValue)) null
    else s.trim
    */
    var newExistingVal = existingValue
    if (existingValue != null && existingValue.trim.length > 0) newExistingVal = existingValue.trim
    if (s == null) newExistingVal
    else if (s.trim.length == 0) newExistingVal
    else if (ignoreValue != null && s.equals(ignoreValue)) newExistingVal
    else s.trim
  }

  def validateDatatype(dataType: String): String = {
    val o = ModelConverters.typeMap.getOrElse(dataType.toLowerCase, dataType)
    LOGGER.debug("validating datatype " + dataType + " against " + ModelConverters.typeMap.size + " keys, got " + o)
    o
  }

  def shoudIncludeModel(modelname: String) = {
    if(SwaggerSpec.baseTypes.contains(modelname.toLowerCase))
      false
    else if(modelname.startsWith("java.lang"))
      false
    else true
  }
}