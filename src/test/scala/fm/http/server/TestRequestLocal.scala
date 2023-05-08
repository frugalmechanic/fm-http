package fm.http.server

import fm.common.IP
import io.netty.handler.codec.http.HttpMethod
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.concurrent.ExecutionContext.Implicits.global

final class TestRequestLocal extends AnyFunSuite with Matchers {

  private def makeDummyRequest(): Request = Request.dummy(IP.empty, HttpMethod.GET, "/dummy")

  private val requestLocal: RequestLocal[String] = new RequestLocal()

  private object requestLocalWithDefault extends RequestLocal[String] {
    override def initialValue(implicit request: Request): Option[String] = Some("default_value")
  }

  test("apply - NoSuchElementException") {
    implicit val request: Request = makeDummyRequest()
    an [NoSuchElementException] should be thrownBy requestLocal()
  }

  test("apply - initialValue") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault() should equal ("default_value")
  }

  test("set - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.set("foo")
    checkValue(requestLocal, "foo")
    requestLocal.set("bar")
    checkValue(requestLocal, "bar")
  }

  test("set - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.set("foo")
    checkValue(requestLocalWithDefault, "foo")
    requestLocalWithDefault.set("bar")
    checkValue(requestLocalWithDefault, "bar")
  }

  test("set - Some - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.set(Some("foo"))
    checkValue(requestLocal, "foo")
    requestLocal.set(Some("bar"))
    checkValue(requestLocal, "bar")
  }

  test("set - Some - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.set(Some("foo"))
    checkValue(requestLocalWithDefault, "foo")
    requestLocalWithDefault.set(Some("bar"))
    checkValue(requestLocalWithDefault, "bar")
  }

  test("set - None - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.set(None)
    requestLocal.hasValue should equal (false)
  }

  test("set - None - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.set(None)
    requestLocalWithDefault.hasValue should equal (false)
  }

  test("setIfNotExists - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.setIfNotExists("foo")
    checkValue(requestLocal, "foo")
  }

  test("setIfNotExists - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.setIfNotExists("foo")
    checkValue(requestLocalWithDefault, "foo")
  }

  test("getOrElseUpdate - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.getOrElseUpdate("foo") should equal ("foo")
    checkValue(requestLocal, "foo")
  }

  test("getOrElseUpdate - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.getOrElseUpdate("foo") should equal ("foo")
    checkValue(requestLocalWithDefault, "foo")
  }

  test("getOrElseUpdate - null - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.getOrElseUpdate(null: String) should equal (null)
    requestLocal.hasValue should equal (false)
  }

  test("getOrElseUpdate - null - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.getOrElseUpdate(null: String) should equal (null)
    requestLocalWithDefault.hasValue should equal (false)
  }

  test("getOrElseUpdate - None - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.getOrElseUpdate(None) should equal (None)
    requestLocal.hasValue should equal (false)
  }

  test("getOrElseUpdate - None - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.getOrElseUpdate(None) should equal (None)
    requestLocalWithDefault.hasValue should equal (false)
  }

  test("remove - requestLocal") {
    implicit val request: Request = makeDummyRequest()
    requestLocal.remove()
    requestLocal.hasValue should equal (false)
    requestLocal.set("foo")
    checkValue(requestLocal, "foo")
    requestLocal.remove()
    requestLocal.hasValue should equal (false)
  }

  test("remove - requestLocalWithDefault") {
    implicit val request: Request = makeDummyRequest()
    requestLocalWithDefault.remove()
    requestLocalWithDefault.hasValue should equal (false)
    requestLocalWithDefault.set("foo")
    checkValue(requestLocalWithDefault, "foo")
    requestLocalWithDefault.remove()
    requestLocalWithDefault.hasValue should equal (false)
  }

  private def checkValue(local: RequestLocal[String], value: String)(implicit request: Request): Unit = {
    local.hasValue should equal(true)
    local.get should equal (Some(value))
    local() should equal (value)
    local.setIfNotExists("BAD_VALUE")
    local() should equal (value)
    local.getOrElseUpdate("BAD_VALUE") should equal (value)
    local.getOrElseUpdate(Some("BAD_VALUE")) should equal (Some(value))
  }

}
