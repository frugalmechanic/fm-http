package fm.http.server

object HttpServerOptions {
  val default: HttpServerOptions = HttpServerOptions(None)
}

/**
 *
 * @param requestIdResponseHeader Set this to include the Request.id in the response headers
 * @param maxRequestsPerConnection The maximum number of requests that we will process per connection
 */
final case class HttpServerOptions(
  requestIdResponseHeader: Option[String],
  maxRequestsPerConnection: Long = Long.MaxValue
) {
  require(maxRequestsPerConnection > 0, "maxRequestsPerConnection must be > 0")
}
