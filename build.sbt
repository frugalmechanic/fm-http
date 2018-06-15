FMPublic

name := "fm-http"

description := "Async Http Client & Server for Scala"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.11.11", "2.12.6")

val fatalWarnings = Seq(
  // Enable -Xlint, but disable the default 'unused' so we can manually specify below
  "-Xlint:-unused",
  // Remove "params" since we often have method signatures that intentionally have the parameters, but may not be used in every implementation, also omit "patvars" since it isn't part of the default xlint:unused and isn't super helpful
  "-Ywarn-unused:imports,privates,locals",
  // Warnings become Errors
  "-Xfatal-warnings"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-language:implicitConversions",
  "-feature",
  "-Xlint",
  "-Ywarn-unused-import"
) ++ (if (scalaVersion.value.startsWith("2.12")) Seq(
  // Scala 2.12 specific compiler flags
  "-opt:l:inline",
  "-opt-inline-from:<sources>"
) else Nil) ++ (if (scalaVersion.value.startsWith("2.12")) fatalWarnings else Nil)

// -Ywarn-unused-import/-Xfatal-warnings casues issues in the REPL and also during doc generation
scalacOptions in (Compile, console) --= fatalWarnings
scalacOptions in (Test, console) --= fatalWarnings
scalacOptions in (Compile, doc) --= fatalWarnings

fork in Test := true

libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "scala-optparse" % "1.1.2",
  "com.frugalmechanic" %% "fm-common" % "0.22.0",
  "com.frugalmechanic" %% "fm-lazyseq" % "0.9.0" % "test"
)

libraryDependencies ++= Seq(
  "io.netty" % "netty-all" % "4.0.56.Final",
  "com.jcraft" % "jzlib" % "1.1.3", // For Netty 4.X
  "com.github.jnr" % "jnr-posix" % "3.0.45", // POSIX Support (getpid and setssid) for the HttpServerApp
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "javax.mail" % "mail" % "1.4.1",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
