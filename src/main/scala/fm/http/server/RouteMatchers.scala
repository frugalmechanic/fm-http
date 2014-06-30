/*
 * Copyright 2014 Frugal Mechanic (http://frugalmechanic.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fm.http.server

import fm.common.Implicits._
import java.util.regex.Pattern
import scala.util.matching.Regex
import io.netty.handler.codec.http.HttpMethod

object RouteMatchers {
//  def main(args: Array[String]): Unit = {
//    "asd/bar/123" match { case simple"$foo/bar/${LONG(num)}" => println(foo+" "+num) }
//    "/asd/foo/bar/more" match { case simple"/$before/foo/$bar/$after" => println(List(before, bar, after)) }
//    "/asd/foo/bar/more" match { case regex"/asd/(?<$foo>.+)" => println(List(foo)) }    
//    "/asd/foo/bar/more/path" match { case simple"/$before/foo/$bar/$after*" => println(List(before, bar, after)) }
//  }
  
  implicit class RichPathMatchers(val sc: StringContext) extends AnyVal {
    /**
     * This support simple patterns of the form:
     *    /path/$part   ==>  Will match /path/foo, /path/bar, etc...
     *    /path/$part*  ==>  Will match /path/to/somewhere, /path/foo/bar, /path/foo, etc...
     *    
     * By default a variable will match everything up to the next / character.
     * If the * is used then the / character is also included.
     * 
     * TODO: make this work with query params.  e.g.:  /path/$var?foo=$foo
     */
    def simple = RichPathMatchingRegex(makeSimpleRegex(sc))
    def p = RichPathMatchingRegex(makeSimpleRegex(sc))
    
    /**
     * This support full regex Patterns with named capturing groups that can be used with Scala String Interpolation
     * 
     * e.g.:
     * 
     *  /path/(?<$rest>.+)  ==> Will match /path/to/somewhere with the "to/somewhere" being bound the the "rest" variable
     */
    def regex = RichPathMatchingRegex(makeFullRegex(sc))
  }
  
  final case class RichPathMatchingRegex(regex: Regex) {
    def unapplySeq(target: Any): Option[List[String]] = target match {
      case path: String => regex.unapplySeq(path)
      case GET(path) => regex.unapplySeq(path)
      case _ => None
    } 
  }
  
  private def makeSimpleRegex(sc: StringContext): Regex = {
    require(sc.parts.nonEmpty, "StringContext.parts is empty!")
    
    sc.parts.reduceLeft{ (res, part) =>
      // If this a wildcard parameter?
      val isWild: Boolean = part.startsWith("*")
      
      // Strip off the * if needed
      val newPart: String = if (isWild) part.substring(1) else part
      
      // The pattern for the paramter
      val paramPattern: String = if (isWild) "(.+)" else "([^/]+)"
      
      res+paramPattern+Pattern.quote(newPart)
    }.r

  }

  /**
   * We support the "(?<name>X)" format for named-capturing group using interpolation:  (?<$name>X)
   */
  private def makeFullRegex(sc: StringContext): Regex = {
    require(sc.parts.nonEmpty, "StringContext.parts is empty!")
    
    var ch: Char = 'a' - 1
    
    sc.parts.reduceLeft{ (res, part) =>
      require(res.endsWith("?<") && part.startsWith(">"), "Invalid Pattern.  Expected Patterns of the form (?<$name>X) for named-capturing groups using Scala Interpolation")
      ch = (ch + 1).toChar
      res+ch+part
    }.r
  }
  
  final case class HttpMethodMatcher(method: HttpMethod) {
    def unapply(request: Request): Option[String] = if (request.method == method) Some(request.path) else None
  }
  
  implicit def StringToDefaultHttpMethodMatcher(s: String): HttpMethodMatcher = GET
  
  //
  // These can be used to extract explicit types from the interpolation matching:
  //    case => simple"/path/${INT(id)}" => call(id)  // id is of type Int
  //
  object INT  { def unapply(x: String): Option[Int]  = x.toIntOption  }
  object LONG { def unapply(x: String): Option[Long] = x.toLongOption }
  
  val GET  = HttpMethodMatcher(HttpMethod.GET)
  val HEAD = HttpMethodMatcher(HttpMethod.HEAD)
  val POST = HttpMethodMatcher(HttpMethod.POST)
  val PUT  = HttpMethodMatcher(HttpMethod.PUT)
}
