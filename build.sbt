name := "playground"

organization := "org.qirx"

lazy val `scala-yaml` = project
  .in( file("scala-yaml") )

lazy val `scala-raml` = project
  .in( file("scala-raml") )
  .dependsOn(`scala-yaml`)

lazy val `play-raml-api-tester` = project
  .in( file("play-raml-api-tester") )
  .dependsOn(`scala-raml`)
  
lazy val `cms` = project
  .in( file("cms") )
