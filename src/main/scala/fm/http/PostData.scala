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

import fm.common.Implicits._
import fm.common.{InputStreamResource, Logging, MultiUseResource, Resource}
import java.io.{File, InputStream}
import java.nio.charset.Charset
import io.netty.buffer.ByteBufInputStream
import io.netty.handler.codec.http.{multipart => netty}

object PostData extends Logging {
  def apply(data: netty.InterfaceHttpData): PostData = data match {
    case d: netty.MemoryAttribute  => MemoryPostAttribute(d)
    case d: netty.DiskAttribute    => DiskPostAttribute(d)
    case d: netty.MemoryFileUpload => MemoryFileUpload(d)
    case d: netty.DiskFileUpload   => DiskFileUpload(d)
    case d: netty.MixedAttribute   => if (d.isInMemory) MemoryPostAttribute(d) else DiskPostAttribute(d)
    case d: netty.MixedFileUpload  => if (d.isInMemory) MemoryFileUpload(d) else DiskFileUpload(d)
    case _ => 
      logger.error("Unknown PostData type: "+data.getClass.getName+"  "+data)
      throw new MatchError(data)
  }
}

sealed trait PostData {
  protected def self: netty.HttpData
  
  /** The POST name for this data */
  final def name: String = Option(self.getName()).getOrElse("")
  
  /** The specified Charset */
  final def charset: Option[Charset] = Option(self.getCharset())
  
  /** The length (in bytes) of this data */
  final def length: Long = self.length()
  
  /** The InputStreamResource for reading this data */
  final def inputStreamResource(): InputStreamResource = inputStreamResource(true, true)
  final def inputStreamResource(autoDecompress: Boolean): InputStreamResource = inputStreamResource(autoDecompress, true)
  def inputStreamResource(autoDecompress: Boolean, autoBuffer: Boolean): InputStreamResource

  final def value: String = self.getString()
  
  /** Force this data to disk (if it's not already there) */
  //def toDiskPostData: DiskPostData
}

sealed trait MemoryPostData extends PostData {
  protected final def resource: Resource[InputStream] = MultiUseResource{ new ByteBufInputStream(self.getByteBuf) }
  
  final def inputStreamResource(autoDecompress: Boolean, autoBuffer: Boolean): InputStreamResource = InputStreamResource(resource, autoDecompress = autoDecompress, autoBuffer = autoBuffer)
}

sealed trait DiskPostData extends PostData {  
  final def file: File = self.getFile()
}

sealed trait PostAttribute extends PostData {
  protected def self: netty.Attribute
}

sealed trait FileUpload extends PostData {
  protected def self: netty.FileUpload
  
  final def fileName: Option[String] = self.getFilename().toBlankOption
  final def contentType: Option[String] = self.getContentType().toBlankOption
  final def contentTransferEncoding: Option[String] = self.getContentTransferEncoding().toBlankOption
}

final case class MemoryPostAttribute(protected val self: netty.Attribute) extends PostAttribute with MemoryPostData {
  require(self.isInMemory, "Can't use an isInMemory=false instance of MemoryAttribute with MemoryPostAttribute")
}

final case class DiskPostAttribute(protected val self: netty.Attribute) extends PostAttribute with DiskPostData {
  require(!self.isInMemory, "Can't use an isInMemory=true instance of DiskAttribute with DiskPostAttribute")
  
  def inputStreamResource(autoDecompress: Boolean, autoBuffer: Boolean): InputStreamResource = {
    InputStreamResource.forFile(file, autoDecompress = autoDecompress, autoBuffer = autoBuffer)
  }
}

final case class MemoryFileUpload(protected val self: netty.FileUpload) extends FileUpload with MemoryPostData {
  require(self.isInMemory, "Can't use an isInMemory=false instance of FileUpload with MemoryFileUpload")
}

final case class DiskFileUpload(protected val self: netty.FileUpload) extends FileUpload with DiskPostData {
  require(!self.isInMemory, "Can't use an isInMemory=true instance of FileUpload with DiskFileUpload")
  
  def inputStreamResource(autoDecompress: Boolean, autoBuffer: Boolean): InputStreamResource = {
    InputStreamResource.forFile(file, fileName.getOrElse(""), autoDecompress = autoDecompress, autoBuffer = autoBuffer)
  }
}
