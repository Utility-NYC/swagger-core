package converter

import com.wordnik.swagger.core.util._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class ClassWrapperTest extends FunSpec with ShouldMatchers {

  describe("resolve type parameters in generic base class") {
    val clazzWrapper = new ClassWrapper(classOf[FinalTypeFixedTypeParam])
    clazzWrapper.getRawClass should be(classOf[FinalTypeFixedTypeParam])

    it("getName stays unchanged after by fromName") {
      var clazz = clazzWrapper
      while (clazz != null) {
        ClassWrapper.forName(clazz.getName).getName should be (clazz.getName)
        clazz = clazz.getSuperclass
      }
    }

    it("resolve result type in method1") {
      val method = clazzWrapper.getMethodReturnType("method1")
      method.getRawClass should be(classOf[StringBuffer])
      method.getName should be(classOf[StringBuffer].getName)
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method2") {
      val method = clazzWrapper.getMethodReturnType("method2")
      method.getRawClass should be(classOf[Seq[_]])
      val typeParamName = method.getRawClass.getTypeParameters()(0).getName
      method.getTypeArgument(typeParamName).getRawClass should be(classOf[StringBuffer])
      method.getName should be ("scala.collection.Seq[java.lang.StringBuffer]")
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }
    it("resolve result type in method3") {
      clazzWrapper.getMethodReturnType("method3").getRawClass should be(classOf[Integer])
    }

    it("resolve result type in method4") {
      val method = clazzWrapper.getMethodReturnType("method4")
      method.getRawClass should be (classOf[Map[_,_]])
      val typeParamNameKey = method.getRawClass.getTypeParameters()(0).getName
      val typeParamNameValue = method.getRawClass.getTypeParameters()(1).getName
      method.getTypeArgument(typeParamNameKey).getRawClass should be (classOf[StringBuffer])
      method.getTypeArgument(typeParamNameValue).getRawClass should be (classOf[Object])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method5") {
      val method = clazzWrapper.getMethodReturnType("method5")
      method.getRawClass should be (classOf[Object])
      method.getRawClass.getTypeParameters.length should be (0)
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method6") {
      val method = clazzWrapper.getMethodReturnType("method6")
      method.isArray should be (true)
      method.getArrayComponent.getName should be ("scala.collection.immutable.List[java.lang.StringBuffer]")
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method11") {
      (clazzWrapper.getMethodReturnType("method11").getRawClass) should be(classOf[String])
    }

    it("resolve result type in method12") {
      val method = clazzWrapper.getMethodReturnType("method12")
      method.getRawClass should be (classOf[Seq[_]])
      val typeParamName = method.getRawClass.getTypeParameters()(0).getName
      method.getTypeArgument(typeParamName).getRawClass should be (classOf[String])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method13") {
      clazzWrapper.getMethodReturnType("method13").getRawClass should be(classOf[Integer])
    }

    it("resolve result type in method14") {
      val method = clazzWrapper.getMethodReturnType("method14")
      method.getRawClass should be (classOf[Map[_,_]])
      val typeParamNameKey = method.getRawClass.getTypeParameters()(0).getName
      val typeParamNameValue = method.getRawClass.getTypeParameters()(1).getName
      method.getTypeArgument(typeParamNameKey).getRawClass should be (classOf[String])
      val valueType = method.getTypeArgument(typeParamNameValue)
      valueType.getRawClass should be (classOf[List[_]])
      val valueTypeName = valueType.getRawClass.getTypeParameters()(0).getName
      valueType.getTypeArgument(valueTypeName).getRawClass should be (classOf[String])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }
  }
  describe("resolve type parameters in generic base class passing type parameter down") {
    val clazzWrapper = new ClassWrapper(classOf[FinalTypePassDownTypeParam])
    clazzWrapper.getRawClass should be (classOf[FinalTypePassDownTypeParam])

    it("getName stays unchanged after by fromName") {
      var clazz = clazzWrapper
      while (clazz != null) {
        ClassWrapper.forName(clazz.getName).getName should be (clazz.getName)
        clazz = clazz.getSuperclass
      }
    }

    it("resolve result type in method1") {
      clazzWrapper.getMethodReturnType("method1").getRawClass should be(classOf[String])
    }

    it("resolve result type in method2") {
      val method = clazzWrapper.getMethodReturnType("method2")
      method.getRawClass should be(classOf[Seq[_]])
      val typeParamName = method.getRawClass.getTypeParameters()(0).getName
      method.getTypeArgument(typeParamName).getRawClass should be(classOf[String])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }
    it("resolve result type in method3") {
      clazzWrapper.getMethodReturnType("method3").getRawClass should be(classOf[Integer])
    }

    it("resolve result type in method4") {
      val method = clazzWrapper.getMethodReturnType("method4")
      method.getRawClass should be (classOf[Map[_,_]])
      val typeParamNameKey = method.getRawClass.getTypeParameters()(0).getName
      val typeParamNameValue = method.getRawClass.getTypeParameters()(1).getName
      method.getTypeArgument(typeParamNameKey).getRawClass should be (classOf[String])
      method.getTypeArgument(typeParamNameValue).getRawClass should be (classOf[Object])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method5") {
      val method = clazzWrapper.getMethodReturnType("method5")
      method.getRawClass should be (classOf[Object])
      method.getRawClass.getTypeParameters.length should be (0)
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method11") {
      (clazzWrapper.getMethodReturnType("method11").getRawClass) should be(classOf[String])
    }

    it("resolve result type in method12") {
      val method = clazzWrapper.getMethodReturnType("method12")
      method.getRawClass should be (classOf[Seq[_]])
      val typeParamName = method.getRawClass.getTypeParameters()(0).getName
      method.getTypeArgument(typeParamName).getRawClass should be (classOf[String])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }

    it("resolve result type in method13") {
      clazzWrapper.getMethodReturnType("method13").getRawClass should be(classOf[Integer])
    }

    it("resolve result type in method14") {
      val method = clazzWrapper.getMethodReturnType("method14")
      method.getRawClass should be (classOf[Map[_,_]])
      val typeParamNameKey = method.getRawClass.getTypeParameters()(0).getName
      val typeParamNameValue = method.getRawClass.getTypeParameters()(1).getName
      method.getTypeArgument(typeParamNameKey).getRawClass should be (classOf[String])
      val valueType = method.getTypeArgument(typeParamNameValue)
      valueType.getRawClass should be (classOf[List[_]])
      val valueTypeName = valueType.getRawClass.getTypeParameters()(0).getName
      valueType.getTypeArgument(valueTypeName).getRawClass should be (classOf[String])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }
  }
  describe("resolve type parameters in non-generic base class") {
    val clazzWrapper = new ClassWrapper(classOf[FinalTypeNonGenericBase])
    clazzWrapper.getRawClass should be (classOf[FinalTypeNonGenericBase])

    it("getName stays unchanged after by fromName") {
      var clazz = clazzWrapper
      while (clazz != null) {
        ClassWrapper.forName(clazz.getName).getName should be (clazz.getName)
        clazz = clazz.getSuperclass
      }
    }

    it("resolve result type in method1") {
      val method = clazzWrapper.getMethodReturnType("method1")
      method.getRawClass should be (classOf[Seq[_]])
      val typeParamName = method.getRawClass.getTypeParameters()(0).getName
      method.getTypeArgument(typeParamName).getRawClass should be (classOf[String])
      ClassWrapper.forName(method.getName).getName should be (method.getName)
    }
    it("resolve result type in method2") {
      clazzWrapper.getMethodReturnType("method2").getRawClass should be(classOf[Integer])
    }
  }
  describe("resolve type parameters in recursive base class") {
    val clazzWrapper = new ClassWrapper(classOf[FinalTypeRecursive])
    clazzWrapper.getRawClass should be (classOf[FinalTypeRecursive])

    it("getName stays unchanged after by fromName") {
      var clazz = clazzWrapper
      while (clazz != null) {
        ClassWrapper.forName(clazz.getName).getName should be (clazz.getName)
        clazz = clazz.getSuperclass
      }
    }

    it("resolve result type in method1") {
      clazzWrapper.getMethodReturnType("method1").getRawClass should be(classOf[FinalTypeRecursive])
    }
  }
}

class GenericBaseClass[TST >: Null] {
  def method1: TST = null
  def method2: Seq[TST] = null
  def method3: Integer = null
  def method4[T2]: Map[TST, T2] = null
  def method5[TST >: Null]: TST = null
  def method6: Array[List[TST]] = null
}

class NonGenericBaseClass {
  def method1: Seq[String] = null
  def method2: Integer = null
}

class GenericMiddleClassPassDownTypeParam[TST >: Null] extends GenericBaseClass[TST] {
  def method11: TST = null
  def method12: Seq[TST] = null
  def method13: Integer = null
  def method14: Map[String, List[TST]] = null
}

class GenericMiddleClassFixedTypeParam[TST >: Null] extends GenericBaseClass[StringBuffer] {
  def method11: TST = null
  def method12: Seq[TST] = null
  def method13: Integer = null
  def method14: Map[String, List[TST]] = null
}

class FinalTypePassDownTypeParam extends GenericMiddleClassPassDownTypeParam[String] {}
class FinalTypeFixedTypeParam extends GenericMiddleClassFixedTypeParam[String] {}
class FinalTypeNonGenericBase extends NonGenericBaseClass {}
class FinalTypeRecursive extends GenericMiddleClassPassDownTypeParam[FinalTypeRecursive] {}

