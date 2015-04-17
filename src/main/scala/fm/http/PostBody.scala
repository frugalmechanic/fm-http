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
package fm.http

import fm.common.QueryParams
import fm.common.Implicits._
import io.netty.handler.codec.http.multipart

object PostBody {
  def fromNetty(datas: Vector[multipart.InterfaceHttpData]): PostBody = apply(datas.map{ PostData(_) })
  
  def apply(datas: Vector[PostData]): PostBody = {
    val attributes: Vector[PostAttribute] = datas.collect{ case a: PostAttribute => a }
    val fileUploads: Vector[FileUpload] = datas.collect{ case f: FileUpload => f }
    
    PostBody(None, attributes, fileUploads)
  }
  
  val empty: PostBody = PostBody(None, Vector.empty, Vector.empty)
}

final case class PostBody(default: Option[PostAttribute], attributes: Vector[PostAttribute], fileUploads: Vector[FileUpload]) {
  val attributesLength: Long = attributes.map{ _.length }.sum
  val fileUploadsLength: Long = fileUploads.map{ _.length }.sum
  val length: Long = attributesLength + fileUploadsLength
  
  def attribute(name: String): Vector[PostAttribute] = attributes.filter{ _.name === name }
  def fileUpload(name: String): Vector[FileUpload] = fileUploads.filter{ _.name === name }
  
  /** Convert the attributes into QueryParams */
  def toQueryParams(): QueryParams = toQueryParams(Long.MaxValue)
  
  /** Convert the attributes into QueryParams as long as the total size of the content is less than maxSizeBytes */  
  def toQueryParams(maxSizeBytes: Long): QueryParams = {
    require(length < maxSizeBytes, s"Size of POST Attributes ($length) exceeds maxSizeBytes ($maxSizeBytes)")
    QueryParams(attributes.map{ a => a.name -> a.value }) 
  }

  
}