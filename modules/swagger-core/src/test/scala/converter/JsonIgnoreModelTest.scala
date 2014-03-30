package converter

import models._

import com.wordnik.swagger.converter._

import com.wordnik.swagger.core.util._

import java.util.Date

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class JsonIgnoreModelTest extends FlatSpec with ShouldMatchers {
  it should "ignore model with JsonIgnore annotation" in {
    val models = ModelConverters.readAll(classOf[ModelWithIgnoreAnnotation])
    JsonSerializer.asJson(models) should be ("""[{"id":"ModelWithIgnoreAnnotation","properties":{"name":{"type":"string"}}}]""")
  }
}
