name := "ScalaR"

organization := ""

version := "0.0.1"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc(),
  "com.quantifind" %% "wisp" % "0.0.4"
  // // Last stable release
  // "org.scalanlp" %% "breeze" % "0.12",
  
  // // Native libraries are not included by default. add this if you want them (as of 0.7)
  // // Native libraries greatly improve performance, but increase jar sizes. 
  // // It also packages various blas implementations, which have licenses that may or may not
  // // be compatible with the Apache License. No GPL code, as best I know.
  // "org.scalanlp" %% "breeze-natives" % "0.12",

  // // The visualization library is distributed separately as well.
  // // It depends on LGPL code
  // "org.scalanlp" %% "breeze-viz" % "0.12"
)

initialCommands := "import scalar._"

