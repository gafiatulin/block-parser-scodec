name := "block-parser-scodec"

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

organization := "com.github.gafiatulin"

developers += Developer("", "Victor Gafiatulin", "gafiatulin@gmail.com", new URL("https://github.com/gafiatulin"))

scmInfo := Some(ScmInfo(
  url("https://github.com/gafiatulin/block-parser-scodec/"),
  "scm:git:github.com:gafiatulin/block-parser-scodec.git"
))

version := "0.1"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import"
)

libraryDependencies ++= Seq(
  "org.scodec"              %% "scodec-core" % "1.10.3",
  "com.madgag.spongycastle" %  "core"        % "1.56.0.0",
  "org.scalatest"           %% "scalatest"   % "3.0.1" % "test"
)

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/Work/mvn-repo")))

crossScalaVersions := Seq("2.11.11", "2.12.3")