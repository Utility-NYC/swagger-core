package com.wordnik.swagger.reader

import com.wordnik.swagger.model._
import java.lang.reflect.{Type, Field, Method}
import java.lang.annotation.Annotation
import com.wordnik.swagger.core.util.ClassWrapper

case class PropertyMetaInfo(
                             returnClass: ClassWrapper,
                             propertyName: String,
                             propertyAnnotations: Array[Annotation],
                             genericType: Type,
                             `type`: Type,
                             isField: Boolean = false
                             )


trait ModelReader {
  def read(modelType: String): Option[Map[String, Model]]

  def parseMethod(cls: ClassWrapper, method: Method, metaInfo: PropertyMetaInfo): PropertyMetaInfo

  def parseField(cls: ClassWrapper, field: Field, metaInfo: PropertyMetaInfo): PropertyMetaInfo

  def processModelProperty(
                            modelProperty: ModelProperty,
                            cls: ClassWrapper,
                            propertyAnnotations: Array[Annotation],
                            fieldAnnotations: Array[Annotation]): ModelProperty
}

class DefaultModelReader extends ModelReader {
  def read(modelType: String): Option[Map[String, Model]] = None

  def parseMethod(cls: ClassWrapper, method: Method, metaInfo: PropertyMetaInfo): PropertyMetaInfo = metaInfo

  def parseField(cls: ClassWrapper, field: Field, metaInfo: PropertyMetaInfo): PropertyMetaInfo = metaInfo

  def processModelProperty(
                            modelProperty: ModelProperty,
                            cls: ClassWrapper,
                            propertyAnnotations: Array[Annotation],
                            fieldAnnotations: Array[Annotation]): ModelProperty = modelProperty
}

object ModelReaders {
  var reader: ModelReader = new DefaultModelReader

  def setReader(reader: ModelReader) = ModelReaders.reader = reader
}
