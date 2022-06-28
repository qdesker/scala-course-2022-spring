package karazin.scala.users.group.week5.homework

import scala.concurrent.Future

import karazin.scala.users.group.week5.homework.givens.{given, _}

/*
  Write test for all programs in karazin.scala.users.group.week4.homework.givens

  Make sure that the following cases are tested:
    • json string representation for integers works
    • json string representation for booleans works
    • json string representation for strings works
    • json string representation for lists for integers, booleans and strings works
    • json string representation for maps fails on compile time

  Review:
    • https://www.json.org/json-en.html
    • https://scalameta.org/munit/docs/tests.html
    • https://scalameta.org/munit/docs/assertions.html
    • https://scalameta.org/munit/docs/assertions.html#compileerrors

  NB: Do not use sync, this homework does not belong async stuff

 */
class GivensSuite extends munit.FunSuite:

  test("Integer") {
    assertEquals(
      JsonStringEncoder[Int]
        .encode(200), "200"
    )
  }

  test("Integer negative") {
    assertEquals(
      JsonStringEncoder[Int]
        .encode(-7), "-7"
    )
  }

  test("Boolean false") {
    assertEquals(
      JsonStringEncoder[Boolean]
        .encode(false), "false"
    )
  }

  test("Boolean true") {
    assertEquals(
      JsonStringEncoder[Boolean]
        .encode(true), "true"
    )
  }

  test("String") {
    assertEquals(
      JsonStringEncoder[String]
        .encode("Contra Spem Spero"), "\"Contra Spem Spero\""
    )
  }

  test("Empty") {
    assertEquals(
      JsonStringEncoder[String]
        .encode(""), "\"\""
    )
  }

  test("List String") {
    assertEquals(
      JsonStringEncoder[List[String]]
        .encode("Contra" :: "Spem" :: "Spero" :: Nil), "[\"Contra\", \"Spem\", \"Spero\"]"
    )
  }

  test("List Booleans") {
    assertEquals(
      JsonStringEncoder[List[Boolean]]
        .encode(true :: false :: true :: Nil), "[true, false, true]"
    )
  }

  test("List Integers") {
    assertEquals(
      JsonStringEncoder[List[Int]]
        .encode(90 :: -119 :: 0 :: Nil), "[1, -2, 0]"
    )
  }

  test("List Empty") {
    assertEquals(
      JsonStringEncoder[List[Int]]
        .encode(Nil), "[]"
    )
  }

  test("Map fails") {
    compileErrors("JsonStringEncoder[Map[String, Int]].encode(Map(\"x\" -> 100, \"y\" -> -2, \"z\" -> 0))")
  }