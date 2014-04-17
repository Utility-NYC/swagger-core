package com.wordnik.swagger.converter

import com.wordnik.swagger.model._
import com.wordnik.swagger.core.{ SwaggerSpec, SwaggerTypes }
import com.wordnik.swagger.core.util.{ModelUtil, ClassWrapper, TypeUtil}
import com.wordnik.swagger.annotations.ApiModelProperty

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonProperty}

import org.slf4j.LoggerFactory

import sun.reflect.generics.reflectiveObjects.{ ParameterizedTypeImpl, TypeVariableImpl }

import java.lang.reflect.{ Type, TypeVariable, Field, Modifier, Method, ParameterizedType }
import java.lang.annotation.Annotation
import javax.xml.bind.annotation._

import scala.collection.mutable.{ LinkedHashMap, ListBuffer, HashSet, HashMap }
import com.wordnik.swagger.reader.{PropertyMetaInfo, ModelReaders}

class ModelPropertyParser(cls: ClassWrapper, t: Map[String, String] = Map.empty) (implicit properties: LinkedHashMap[String, ModelProperty]) {
  private val LOGGER = LoggerFactory.getLogger(classOf[ModelPropertyParser])

  import ModelUtil._

  val typeMap = {
    if(t.isEmpty)
      SwaggerTypes.primitives
    else t
  }
  val processedFields = new ListBuffer[String]
  val excludedFieldTypes = new HashSet[String]
  final val positiveInfinity = "Infinity"
  final val negativeInfinity = "-Infinity"

  def parse = Option(cls).map(parseRecursive(_))

  def parseRecursive(hostClass: ClassWrapper): Unit = {
    if(!hostClass.isEnum) {
      LOGGER.debug("processing class " + hostClass)
      for (method <- hostClass.getDeclaredMethods) {
        if (Modifier.isPublic(method.getModifiers()) && !Modifier.isStatic(method.getModifiers()) && !method.isSynthetic())
          parseMethod(hostClass, method)
      }
      for (field <- hostClass.getDeclaredFields) {
        if (Modifier.isPublic(field.getModifiers()) && !Modifier.isStatic(field.getModifiers()) && !field.isSynthetic())
          parseField(hostClass, field)
      }
      Option(hostClass.getSuperclass).map(parseRecursive(_))
    }
    else {
      LOGGER.debug("Not processing enum class " + hostClass)
    }
  }

  def parseField(hostClass: ClassWrapper, field: Field) = {
    LOGGER.debug("processing field " + hostClass + "." + field)

    val propertyMetaInfo = new PropertyMetaInfo(hostClass.getFieldType(field), field.getName, field.getAnnotations)
    val newMetaInfo = ModelReaders.reader.parseField(hostClass, field, propertyMetaInfo)
    parsePropertyAnnotations(newMetaInfo)
  }

  def parseMethod(hostClass: ClassWrapper, method: Method) = {
    if (method.getParameterTypes == null || method.getParameterTypes.length == 0) {
      LOGGER.debug("processing method " + method)

      val propertyMetaInfo = new PropertyMetaInfo(hostClass.getMethodReturnType(method), method.getName, method.getAnnotations)
      val newMetaInfo = ModelReaders.reader.parseMethod(hostClass, method, propertyMetaInfo)
      parsePropertyAnnotations(newMetaInfo)
    }
  }

  def extractGetterProperty(methodFieldName: String): (String, Boolean) = {
    if (methodFieldName != null &&
      (methodFieldName.startsWith("get")) &&
      methodFieldName.length > 3) {
      (methodFieldName.substring(3, 4).toLowerCase() + methodFieldName.substring(4, methodFieldName.length()), true)
    } else if (methodFieldName != null &&
      (methodFieldName.startsWith("is")) &&
      methodFieldName.length > 2) {
      (methodFieldName.substring(2, 3).toLowerCase() + methodFieldName.substring(3, methodFieldName.length()), true)
    } else {
      (methodFieldName, false)
    }
  }

  def parsePropertyAnnotations(metaInfo : PropertyMetaInfo): Any = {
    if (metaInfo != null) {
      parsePropertyAnnotations(metaInfo.returnClass, metaInfo.propertyName, metaInfo.propertyAnnotations)
    }
  }

  def parsePropertyAnnotations(returnClass: ClassWrapper, propertyName: String, propertyAnnotations: Array[Annotation]): Any = {
    val e = extractGetterProperty(propertyName)
    var originalName = e._1
    var isGetter = e._2

    var isFieldExists = false
    var isJsonProperty = false
    var hasAccessorNoneAnnotation = false

    var processedAnnotations = processAnnotations(originalName, propertyAnnotations)
    var name = processedAnnotations("name").asInstanceOf[String]

    var required = processedAnnotations("required").asInstanceOf[Boolean]
    var position = processedAnnotations("position").asInstanceOf[Int]

    var description = {
      if(processedAnnotations.contains("description") && processedAnnotations("description") != null)
        Some(processedAnnotations("description").asInstanceOf[String])
      else None
    }
    var isTransient = processedAnnotations("isTransient").asInstanceOf[Boolean]
    var isXmlElement = processedAnnotations("isXmlElement").asInstanceOf[Boolean]
    val isDocumented = processedAnnotations("isDocumented").asInstanceOf[Boolean]
    var allowableValues = {
      if(returnClass.isEnum) 
        Some(AllowableListValues((for(v <- returnClass.getEnumConstants) yield v.toString).toList))
      else
        processedAnnotations("allowableValues").asInstanceOf[Option[AllowableValues]]
    }

    var fieldAnnotations: Array[Annotation] = null
    try {
      fieldAnnotations = getDeclaredField(this.cls, originalName).getAnnotations()

      var propAnnoOutput = processAnnotations(originalName, fieldAnnotations)
      var propPosition = propAnnoOutput("position").asInstanceOf[Int]

      if (name == null || name.equals(originalName)) {
        name = propAnnoOutput("name").asInstanceOf[String]
      }
      if(allowableValues == None) 
        allowableValues = propAnnoOutput("allowableValues").asInstanceOf[Option[AllowableValues]]
      if(description == None && propAnnoOutput.contains("description") && propAnnoOutput("description") != null) 
        description = Some(propAnnoOutput("description").asInstanceOf[String])
      if(propPosition != 0) position = propAnnoOutput("position").asInstanceOf[Int]
      if(required == false) required = propAnnoOutput("required").asInstanceOf[Boolean]
      isFieldExists = true
      if (!isTransient) isTransient = propAnnoOutput("isTransient").asInstanceOf[Boolean]
      if (!isXmlElement) isXmlElement = propAnnoOutput("isXmlElement").asInstanceOf[Boolean]
      isJsonProperty = propAnnoOutput("isJsonProperty").asInstanceOf[Boolean]
    } catch {
      //this means there is no field declared to look for field level annotations.
      case e: java.lang.NoSuchFieldException => {
        // isTransient = false
      }
    }

    if (name == null) name = originalName

    //if class has accessor none annotation, the method/field should have explicit xml element annotations, if not
    // consider it as transient
    if (!isXmlElement && hasAccessorNoneAnnotation)
      isTransient = true

    if (!(isTransient && !isXmlElement && !isJsonProperty) && name != null && (isFieldExists || isGetter || isDocumented)) {
      var paramType = getDataType(returnClass, false)
      LOGGER.debug("inspecting " + paramType)
      var simpleName = getDataType(returnClass, true)

      if (!"void".equals(paramType) && null != paramType && !processedFields.contains(name)) {
        if(!excludedFieldTypes.contains(paramType)) {
          val items = {
            val ComplexTypeMatcher = "([a-zA-Z]*)\\[([a-zA-Z\\.\\-0-9_]*)\\].*".r
            paramType match {
              case ComplexTypeMatcher(containerType, basePart) => {
                LOGGER.debug("containerType: " + containerType + ", basePart: " + basePart + ", simpleName: " + simpleName)
                paramType = containerType
                val ComplexTypeMatcher(t, simpleTypeRef) = simpleName
                val typeRef = {
                  if(simpleTypeRef.indexOf(",") > 0) // it's a map, use the value only
                    simpleTypeRef.split(",").last
                  else simpleTypeRef
                }
                simpleName = containerType
                if(isComplex(simpleTypeRef)) {
                  Some(ModelRef(null, Some(simpleTypeRef), Some(basePart)))
                }
                else Some(ModelRef(simpleTypeRef, None, Some(basePart)))
              }
              case _ => None
            }
          }
          var param = ModelProperty(
            validateDatatype(simpleName),
            paramType,
            position,
            required,
            description,
            allowableValues.getOrElse(AnyAllowableValues),
            items)
          param = ModelReaders.reader.processModelProperty(param, returnClass, propertyAnnotations, fieldAnnotations)
          LOGGER.debug("added param type " + paramType + " for field " + name)
          properties += name -> param
        }
        else {
          LOGGER.debug("field " + paramType + " is has been explicitly excluded")
        }
      }
      else {
        LOGGER.debug("skipping " + name)
      }
      processedFields += name
    }
  }

  def isComplex(typeName: String): Boolean = {
    !SwaggerSpec.baseTypes.contains(typeName.toLowerCase)
  }

  def processAnnotations(name: String, annotations: Array[Annotation]): HashMap[String, Any] = {
    var isTransient = false
    var isXmlElement = false
    var isDocumented = false
    var isJsonProperty = false

    var classname = name
    var updatedName = name
    var required = false
    var defaultValue: String = null
    var description: String = null
    var notes: String = null
    var paramType: String = null
    var allowableValues: Option[AllowableValues] = None
    var paramAccess: String = null
    var wrapperName: String = null
    var position = 0

    for (ma <- annotations) {
      ma match {
        case e: XmlTransient => isTransient = true
        case e: ApiModelProperty => {
          description = readString(e.value)
          notes = readString(e.notes)
          paramType = readString(e.dataType)
          if(e.required) required = true
          if(e.position != 0) position = e.position
          isDocumented = true
          allowableValues = Some(toAllowableValues(e.allowableValues))
          paramAccess = readString(e.access)
        }
        case e: XmlAttribute => {
          updatedName = readString(e.name, name, "##default")
          updatedName = readString(name, name)
          if(e.required) required = true
          isXmlElement = true
        }
        case e: XmlElement => {
          updatedName = readString(e.name, name, "##default")
          // updatedName = readString(name, name)
          defaultValue = readString(e.defaultValue, defaultValue, "\u0000")

          required = e.required
          val xmlElementTypeMethod = classOf[XmlElement].getDeclaredMethod("type")
          val typeValueObj = xmlElementTypeMethod.invoke(e)
          val typeValue = {
            if (typeValueObj == null) null
            else typeValueObj.asInstanceOf[Class[_]]
          }
          isXmlElement = true
        }
        case e: XmlElementWrapper => wrapperName = readString(e.name, wrapperName, "##default")
        case e: JsonIgnore => isTransient = true
        case e: JsonProperty => {
          updatedName = readString(e.value, name)
          isJsonProperty = true
        }
        case _ => 
      }
    }
    val output = new HashMap[String, Any]
    output += "isTransient" -> isTransient
    output += "isXmlElement" -> isXmlElement
    output += "isDocumented" -> isDocumented
    output += "isJsonProperty" -> isJsonProperty
    output += "name" -> updatedName
    output += "required" -> required
    output += "defaultValue" -> defaultValue
    output += "description" -> description
    output += "notes" -> notes
    output += "paramType" -> paramType
    output += "allowableValues" -> allowableValues
    output += "paramAccess" -> paramAccess
    output += "position" -> position
    output
  }

  def getDeclaredField(inputClass: ClassWrapper, fieldName: String): Field = {
    try {
      inputClass.getDeclaredField(fieldName)
    } catch {
      case t: NoSuchFieldException => {
        if (inputClass.getSuperclass != null && inputClass.getSuperclass.getName != "java.lang.Object") {
          getDeclaredField(inputClass.getSuperclass, fieldName)
        } else {
          throw t
        }
      }
    }
  }

  def toAllowableValues(csvString: String, paramType: String = null): AllowableValues = {
    if (csvString.toLowerCase.startsWith("range[")) {
      val ranges = csvString.substring(6, csvString.length() - 1).split(",")
      toAllowableRange(ranges, csvString)
    } else if (csvString.toLowerCase.startsWith("rangeexclusive[")) {
      val ranges = csvString.substring(15, csvString.length() - 1).split(",")
      toAllowableRange(ranges, csvString)
    } else {
      if (csvString == null || csvString.length == 0) {
        AnyAllowableValues
      } else {
        val params = csvString.split(",").toList
        AllowableListValues(params)
      }
    }
  }

  def toAllowableRange(ranges: Array[String], inputStr: String): AllowableValues = {
    if (ranges.size < 2) {
      LOGGER.error("invalid range input")
      AnyAllowableValues
    }
    else {
      val min = ranges(0) match {
        case e: String if(e == positiveInfinity) => Float.PositiveInfinity
        case e: String if(e == negativeInfinity) => Float.NegativeInfinity
        case e: String => e.toFloat
      }
      val max = ranges(1) match {
        case e: String if(e == positiveInfinity) => Float.PositiveInfinity
        case e: String if(e == negativeInfinity) => Float.NegativeInfinity
        case e: String => e.toFloat
      }
      AllowableRangeValues(min.toString, max.toString)
    }
  }

}