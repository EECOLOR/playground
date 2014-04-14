name := "scala-raml"

organization := "org.qirx"

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)
