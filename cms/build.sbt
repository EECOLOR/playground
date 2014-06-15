name := "cms"

organization := "org.qirx"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.3.0",
  "org.qirx" %% "little-spec" % "0.3" % "test"
)

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework")
