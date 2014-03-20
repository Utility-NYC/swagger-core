package com.wordnik.swagger.reader

import com.wordnik.swagger.model._
import java.lang.reflect.{Type, Field, Method}
import java.lang.annotation.Annotation

case class PropertyMetaInfo(
  returnClass: Class[_],
  propertyName: String,
  propertyAnnotations: Array[Annotation],
  genericReturnType: Type,
  returnType: Type)

trait ModelReader {
  def read(modelType : String) : Option[Map[String, Model]]
  def parseMethod(method : Method, metaInfo : PropertyMetaInfo) : PropertyMetaInfo
  def parseField(field : Field, metaInfo : PropertyMetaInfo) : PropertyMetaInfo
}

class DefaultModelReader extends ModelReader {
  def read(modelType : String) : Option[Map[String, Model]] = None
  def parseMethod(method : Method, metaInfo : PropertyMetaInfo) : PropertyMetaInfo = metaInfo
  def parseField(field : Field, metaInfo : PropertyMetaInfo) : PropertyMetaInfo = metaInfo
}

object ModelReaders {
  var reader: ModelReader = new DefaultModelReader

  def setReader(reader: ModelReader) = ModelReaders.reader = reader
}
