package com.wordnik.swagger.converter

import com.wordnik.swagger.core.SwaggerSpec
import com.wordnik.swagger.annotations.ApiModel
import com.wordnik.swagger.core.util.{ModelUtil, ClassWrapper}

trait BaseConverter {
  def toDescriptionOpt(cls: ClassWrapper): Option[String] = {
    var description: Option[String] = None
    for(anno <- cls.getAnnotations) {
      anno match {
        case e: ApiModel => {
          if(e.description != null) description = Some(e.description)
        }
        case _ =>        
      }
    }
    description
  }

  def toName(cls: ClassWrapper): String = {
    ModelUtil.toName(cls)
  }
}