package fm.http.server

object HttpServerOptions {
  val default: HttpServerOptions = HttpServerOptions(None)
}

/**
 *
 * @param requestIdResponseHeader Set this to include the Request.id in the response headers
 */
final case class HttpServerOptions(
  requestIdResponseHeader: Option[String]
) {

}
