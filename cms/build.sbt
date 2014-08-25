name := "cms"

organization := "org.qirx"

scalaVersion := "2.11.2"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play" % "2.3.0",
  "com.typesafe.play" %% "play-test" % "2.3.0",
  "com.typesafe.play" %% "play-json" % "2.3.0",
  "com.typesafe.play" %% "play-ws" % "2.3.0",
  "org.qirx" %% "little-spec" % "0.4-SNAPSHOT" % "test",
  "org.qirx" %% "little-spec-extra-documentation" % "0.4-SNAPSHOT" % "test"
)

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework")
