name := "scala-yaml"

organization := "org.qirx"

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

libraryDependencies += "org.qirx" %% "little-spec" % "0.3" % "test"

testFrameworks += new TestFramework("org.qirx.littlespec.TestFramework")
