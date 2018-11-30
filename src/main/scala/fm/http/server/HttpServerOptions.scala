package fm.http.server

import fm.http.Headers

object HttpServerOptions {
  /**
   * This is the original default behavior of using the last value of the "X-Forwarded-For" header
   */
  val defaultClientIPLookupSpec: ClientIPLookupSpec = ClientIPLookupSpec(
    headerName = Headers.NonStandardNames.X_FORWARDED_FOR.toString,
    requiredHeaderAndValue = None,
    valueToUse = ClientIPHeaderValueToUse.Last
  )

  val defaultMaxInitialLineLength: Int = 4096
  val defaultMaxHeaderSize: Int = 8192
  val defaultMaxChunkSize: Int = 8192

  val default: HttpServerOptions = HttpServerOptions(None)

  /**
   * This allows you to override the Request.remoteIp with an IP Address from an HTTP header field
   *
   * Example:
   *
   * {{{
   *   ClientIPLookupSpec(
   *     headerName = "X-Client-IP",
   *     requiredHeaderAndValue = Some(("X-Is-From-CDN", "secret value set by the CDN/Proxy"))
   *     valueIdx = ClientIPHeaderValueToUse.Last
   *   )
   * }}}
   *
   * In this case if a request comes in with an "X-Client-IP" value but no "X-Is-From-CDN" header value then the
   * "X-Client-IP" will not be used.  If the "X-Client-IP" header does exist and is set to whatever we configured
   * then the "X-Client-IP" will be used.
   *
   * @param headerName The header name to grab the client ip from (e.g. X-Forwarded-For)
   * @param requiredHeaderAndValue An optional header/value pair that must exist in order to look at the headerName
   * @param valueToUse Which value to use when there are multiple values (e.g. X-Forward-For: 1.2.3.4,4.5.6.7)
   */
  final case class ClientIPLookupSpec(
    headerName: String,
    requiredHeaderAndValue: Option[(String,String)],
    valueToUse: ClientIPHeaderValueToUse
  )

  object ClientIPHeaderValueToUse {
    /**
     * Use the first value in the list
     *
     * e.g. 1.2.3.4,5.6.7.8 => 1.2.3.4
     */
    case object First extends ClientIPHeaderValueToUse

    /**
     * Use the last value in the list
     *
     * e.g. 1.2.3.4,5.6.7.8 => 5.6.7.8
     */
    case object Last extends ClientIPHeaderValueToUse

    /**
     * Use the value at the passed in index offset (or next available value)
     *
     * e.g. idx=0 1.2.3.4,5.6.7.8,9.10.11.12 => 1.2.3.4 (same as First)
     * e.g. idx=1 1.2.3.4,5.6.7.8,9.10.11.12 => 5.6.7.8
     * e.g. idx=2 1.2.3.4,5.6.7.8,9.10.11.12 => 9.10.11.12
     * e.g. idx=5 1.2.3.4,5.6.7.8,9.10.11.12 => 9.10.11.12 (use next available value)
     */
    final case class OffsetFromFirst(idx: Int) extends ClientIPHeaderValueToUse

    /**
     * Reverse the list and use the value at the passed in index offset
     *
     * e.g. idx=0 1.2.3.4,5.6.7.8,9.10.11.12 => 9.10.11.12 (same as Last)
     * e.g. idx=1 1.2.3.4,5.6.7.8,9.10.11.12 => 5.6.7.8
     * e.g. idx=2 1.2.3.4,5.6.7.8,9.10.11.12 => 1.2.3.4
     * e.g. idx=5 1.2.3.4,5.6.7.8,9.10.11.12 => 1.2.3.4 (use next available value)
     */
    final case class OffsetFromLast(idx: Int) extends ClientIPHeaderValueToUse
  }

  private def defaultMaxRequestsPerConnection: Long = Long.MaxValue

  sealed trait ClientIPHeaderValueToUse

  private case class Impl(
    requestIdResponseHeader: Option[String],
    maxRequestsPerConnection: Long,
    clientIPLookupSpecs: Seq[HttpServerOptions.ClientIPLookupSpec],
    maxInitialLineLength: Int,
    maxHeaderSize: Int,
    maxChunkSize: Int,
  ) extends HttpServerOptions {

    def withRequestIdResponseHeader(value: Option[String]): HttpServerOptions = copy(requestIdResponseHeader = value)
    def withMaxRequestsPerConnection(value: Long): HttpServerOptions = copy(maxRequestsPerConnection = value)
    def withClientIPLookupSpecs(value: Seq[HttpServerOptions.ClientIPLookupSpec]): HttpServerOptions = copy(clientIPLookupSpecs = value)
    def withMaxInitialLineLength(value: Int): HttpServerOptions = copy(maxInitialLineLength = value)
    def withMaxHeaderSize(value: Int): HttpServerOptions = copy(maxHeaderSize = value)
    def withMaxChunkSize(value: Int): HttpServerOptions = copy(maxChunkSize = value)

    require(maxRequestsPerConnection > 0, s"maxRequestsPerConnection must be > 0 ${maxRequestsPerConnection}")
  }

  def apply(): HttpServerOptions = apply(None)
  def apply(requestIdResponseHeader: Option[String]): HttpServerOptions = apply(requestIdResponseHeader, defaultMaxRequestsPerConnection)

  def apply(requestIdResponseHeader: Option[String], maxRequestsPerConnection: Long): HttpServerOptions =
    apply(requestIdResponseHeader, maxRequestsPerConnection, Seq(HttpServerOptions.defaultClientIPLookupSpec))

  def apply(requestIdResponseHeader: Option[String], maxRequestsPerConnection: Long, clientIPLookupSpecs: Seq[ClientIPLookupSpec]): HttpServerOptions =
    apply(requestIdResponseHeader, maxRequestsPerConnection, Seq(HttpServerOptions.defaultClientIPLookupSpec), defaultMaxInitialLineLength, defaultMaxHeaderSize, defaultMaxChunkSize)

  /**
    *
    * @param requestIdResponseHeader Set this to include the Request.id in the response headers
    * @param maxRequestsPerConnection The maximum number of requests that we will process per connection.
    * @param clientIPLookupSpecs How to lookup the client IP from headers (in priority order)
    * @param maxInitialLineLength HttpRequestDecoder maxInitialLineLength
    * @param maxHeaderSize HttpRequestDecoder maxHeaderSize
    * @param maxChunkSize HttpRequestDecoder maxChunkSize
    */
  def apply(
    requestIdResponseHeader: Option[String],
    maxRequestsPerConnection: Long,
    clientIPLookupSpecs: Seq[ClientIPLookupSpec],
    maxInitialLineLength: Int,
    maxHeaderSize: Int,
    maxChunkSize: Int,
  ): HttpServerOptions = Impl(requestIdResponseHeader, maxRequestsPerConnection, clientIPLookupSpecs, maxInitialLineLength, maxHeaderSize, maxChunkSize)
}

trait HttpServerOptions {
  def requestIdResponseHeader: Option[String]
  def maxRequestsPerConnection: Long
  def clientIPLookupSpecs: Seq[HttpServerOptions.ClientIPLookupSpec]
  def maxInitialLineLength: Int
  def maxHeaderSize: Int
  def maxChunkSize: Int

  def withRequestIdResponseHeader(value: Option[String]): HttpServerOptions
  def withMaxRequestsPerConnection(value: Long): HttpServerOptions
  def withClientIPLookupSpecs(value: Seq[HttpServerOptions.ClientIPLookupSpec]): HttpServerOptions
  def withMaxInitialLineLength(value: Int): HttpServerOptions
  def withMaxHeaderSize(value: Int): HttpServerOptions
  def withMaxChunkSize(value: Int): HttpServerOptions
}