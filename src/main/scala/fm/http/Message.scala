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

import io.netty.buffer.ByteBuf
import java.io.{File, InputStream}

/**
 * Represents an HTTP Message (either a Request or Response)
 */
trait Message {
  def headers: Headers
}

trait FullMessage extends Message {
  /** The complete body */
  def buf: ByteBuf
}

trait AsyncMessage extends Message {
  /** The head of the body */
  def head: LinkedHttpContent
}

trait FileMessage extends Message {
  /** This file is the body */
  def file: File
}

trait InputStreamMessage extends Message {
  /** The input stream is the body */
  def input: InputStream
}