package com.wordnik.swagger.converter

import com.wordnik.swagger.model._
import com.wordnik.swagger.core.util.ClassWrapper
import scala.collection.mutable

class MapConverter extends ModelConverter with BaseConverter {

  def read(cls: ClassWrapper, typeMap: Map[String, String]): Option[Model] = {
    if (cls.getRawClass.getSimpleName == "Map") {
      Some(Model(
        toName(cls),
        toName(cls),
        cls.getName,
        new mutable.LinkedHashMap[String, ModelProperty]()
      ))
    } else {
      None
    }
  }
}
