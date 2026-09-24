name := "arl2cpg"

dependsOn(
  Projects.dataflowengineoss % "test->test",
  Projects.x2cpg             % "compile->compile;test->test",
  Projects.javasrc2cpg       % "compile->compile;test->test",
  Projects.linterRules % ScalafixConfig
)

libraryDependencies ++= Seq(
  "org.antlr"      %  "antlr4-runtime"     % Versions.antlr,
  "io.shiftleft"   %% "codepropertygraph"  % Versions.cpg,
  "org.scalatest"  %% "scalatest"          % Versions.scalatest % Test
)

enablePlugins(Antlr4Plugin, JavaAppPackaging, LauncherJarPlugin)

Antlr4 / antlr4PackageName := Some("io.joern.arl2cpg.parser")
Antlr4 / antlr4Version     := Versions.antlr
Antlr4 / javaSource        := (Compile / sourceManaged).value
Compile / doc / sources ~= (_ filter (_ => false))
