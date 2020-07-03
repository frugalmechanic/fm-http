package fm.http.server

import fm.http.Headers

object HttpServerOptions {
  val default: HttpServerOptions = HttpServerOptions(None)

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

  // maxChunkSize Notes:
  //   There is some chatter about this config not being useful and it going away in Netty 5.x and at least one
  //   example of a library setting this to Int.MaxValue.  So let's set this to 1 MB for now.
  val defaultMaxChunkSize: Int = 1048576

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

  sealed trait ClientIPHeaderValueToUse
}

/**
 *
 * @param requestIdResponseHeader Set this to include the Request.id in the response headers
 * @param maxRequestsPerConnection The maximum number of requests that we will process per connection
 * @param clientIPLookupSpecs How to lookup the client IP from headers (in priority order)
 * @param maxInitialLineLength HttpRequestDecoder maxInitialLineLength (defaults to 4096)
 * @param maxHeaderSize HttpRequestDecoder maxHeaderSize (defaults to 8192)
 * @param maxChunkSize HttpRequestDecoder maxChunkSize (defaults to 8192)
 * @param requestHandlerExecutionContextProvider An optional RequestExecutionContextProvider instance to control which
 *                                               ExecutionContext gets passed into the RequestHandler.  Defaults to
 *                                               using the worker EventLoopGroup.
 */
final case class HttpServerOptions(
  requestIdResponseHeader: Option[String],
  maxRequestsPerConnection: Long = Long.MaxValue,
  clientIPLookupSpecs: Seq[HttpServerOptions.ClientIPLookupSpec] = Seq(HttpServerOptions.defaultClientIPLookupSpec),
  maxInitialLineLength: Int = HttpServerOptions.defaultMaxInitialLineLength,
  maxHeaderSize: Int = HttpServerOptions.defaultMaxHeaderSize,
  maxChunkSize: Int = HttpServerOptions.defaultMaxChunkSize,
  requestHandlerExecutionContextProvider: Option[RequestHandlerExecutionContextProvider] = None
) {
  require(maxRequestsPerConnection > 0, "maxRequestsPerConnection must be > 0")
}
