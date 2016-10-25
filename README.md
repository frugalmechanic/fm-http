Frugal Mechanic Async HTTP Client and Server
============================================

[![Build Status](https://travis-ci.org/frugalmechanic/fm-http.svg?branch=master)](https://travis-ci.org/frugalmechanic/fm-http)

This is our Async Http Client & Server for Scala based on Netty

Basic Server Usage
------------------

    import fm.http.server.{HttpServer, RequestRouter, Response}
    import fm.http.server.RouteMatchers._
    import scala.concurrent.Future
    
    val router: RequestRouter = RequestRouter{
      case GET("/") => Future.successful(Response.Ok("Home Page"))
    }
    
    val server: HttpServer = HttpServer(8080, router, "<auth_key>")

Basic Client Usage
------------------

    import fm.http.client.{FullStringResponse, HttpClient, Response}
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global
    
    // Create an HttpClient with default options
    val client: HttpClient = HttpClient()
    
    val res: Future[FullStringResponse] = client.getFullString("http://localhost:8080/")
    
    res.foreach{ r: FullStringResponse => assert(r.body == "Home Page") }
    

Authors
-------

Tim Underwood (<a href="https://github.com/tpunder" rel="author">GitHub</a>, <a href="https://www.linkedin.com/in/tpunder" rel="author">LinkedIn</a>, <a href="https://twitter.com/tpunder" rel="author">Twitter</a>, <a href="https://plus.google.com/+TimUnderwood0" rel="author">Google Plus</a>)

Copyright
---------

Copyright [Frugal Mechanic](http://frugalmechanic.com)

License
-------

[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt)