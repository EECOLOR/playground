name := "playground"

organization := "org.qirx"

lazy val `little-spec` = project
  .in( file("little-spec") )

lazy val `little-spec-specification` = project
  .in( file("little-spec/specification") )
  .dependsOn(`little-spec`)  

test in Test in `little-spec` <<= (test in Test in `little-spec`).dependsOn(compile in Compile in `little-spec-specification`)  
  
lazy val `scala-yaml` = project
  .in( file("scala-yaml") )
  .dependsOn( `little-spec` % "test" )

lazy val `scala-raml` = project
  .in( file("scala-raml") )
  .dependsOn(
    `scala-yaml`,
    `little-spec` % "test")

lazy val `play-raml-api-tester` = project
  .in( file("play-raml-api-tester") )
  .dependsOn(
    `scala-raml`,
    `little-spec` % "test")
