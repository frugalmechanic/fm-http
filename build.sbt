name := "fm-http"

description := "Async Http Client & Server for Scala"

scalaVersion := "3.2.2"

crossScalaVersions := Seq("3.2.2", "2.13.10", "2.12.17", "2.11.12")

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
) ++ (if (scalaVersion.value.startsWith("2.11")) Seq(
  // Scala 2.11 specific compiler flags
  "-Ywarn-unused-import"
) else Nil) ++ (if (scalaVersion.value.startsWith("2.12") || scalaVersion.value.startsWith("2.13")) Seq(
  // Scala 2.12/2.13 specific compiler flags
  "-opt:l:inline",
  "-opt-inline-from:<sources>"
) ++ fatalWarnings else Nil) ++ (if (scalaVersion.value.startsWith("3")) Seq(
  //"-Yno-decode-stacktraces"
  "-explain"
) else Nil)

// -Ywarn-unused-import/-Xfatal-warnings casues issues in the REPL and also during doc generation
Compile / console / scalacOptions --= fatalWarnings
Test / console / scalacOptions --= fatalWarnings
Compile / doc / scalacOptions --= fatalWarnings

Test / fork := true

libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "scala-optparse" % "1.2.1",
  "com.frugalmechanic" %% "fm-common" % "1.0.1",
  "com.frugalmechanic" %% "fm-lazyseq" % "1.0.0" % Test
)

val nettyVersion: String = "4.1.80.Final"

libraryDependencies ++= Seq(
  "io.netty" % "netty-all" % nettyVersion,
  "io.netty" % "netty-transport-native-epoll" % nettyVersion classifier "linux-x86_64",
  "io.netty" % "netty-transport-native-epoll" % nettyVersion classifier "linux-aarch_64",
  "io.netty" % "netty-transport-native-kqueue" % nettyVersion classifier "osx-x86_64",
  "io.netty" % "netty-transport-native-kqueue" % nettyVersion classifier "osx-aarch_64",
  "com.jcraft" % "jzlib" % "1.1.3", // For Netty 4.X
  "com.github.jnr" % "jnr-posix" % "3.0.42", // POSIX Support (getpid and setssid) for the HttpServerApp
  "org.slf4j" % "slf4j-api" % "2.0.6",
  "ch.qos.logback" % "logback-classic" % "1.3.5",
  "javax.mail" % "mail" % "1.4.1",
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)

publishTo := sonatypePublishToBundle.value
