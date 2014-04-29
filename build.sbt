name := "fm-http"

organization := "com.frugalmechanic"

version := "0.2-SNAPSHOT"

description := "Async Http Client & Server for Scala"

licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/frugalmechanic/fm-http"))

scalaVersion := "2.10.4"

// Note: Use "++ 2.11.0" to select a specific version when building
crossScalaVersions := Seq("2.10.4", "2.11.0")

scalacOptions := Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature", "-optimise")

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "scala-optparse" % "1.1.1",
  "com.frugalmechanic" %% "fm-common" % "0.1",
  "com.frugalmechanic" %% "fm-lazyseq" % "0.1" % "test"
)

libraryDependencies ++= Seq(
  "io.netty" % "netty-all" % "4.0.18.Final",
  "com.jcraft" % "jzlib" % "1.1.3", // For Netty 4.X
  "com.github.jnr" % "jnr-posix" % "3.0.1",             // POSIX Support (getpid and setssid) for the HttpServerApp
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.5",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "javax.mail" % "mail" % "1.4.1",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test"
)

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>tim</id>
      <name>Tim Underwood</name>
      <email>tim@frugalmechanic.com</email>
      <organization>Frugal Mechanic</organization>
      <organizationUrl>http://frugalmechanic.com</organizationUrl>
    </developer>
  </developers>
  <scm>
      <connection>scm:git:git@github.com:frugalmechanic/fm-http.git</connection>
      <developerConnection>scm:git:git@github.com:frugalmechanic/fm-http.git</developerConnection>
      <url>git@github.com:frugalmechanic/fm-http.git</url>
  </scm>)

