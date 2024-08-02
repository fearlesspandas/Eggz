ThisBuild / scalaVersion := "3.3.1"
// https://mvnrepository.com/artifact/dev.zio/zio
libraryDependencies += "dev.zio" %% "zio" % "2.1.6"

// https://mvnrepository.com/artifact/io.d11/zhttp
libraryDependencies += "dev.zio" %% "zio-http" % "3.0.0-RC3"
// https://mvnrepository.com/artifact/dev.zio/zio-json
libraryDependencies += "dev.zio" %% "zio-json" % "0.6.2"
libraryDependencies += "dev.zio" %% "zio-profiling" % "0.3.1"
libraryDependencies += "dev.zio" %% "zio-streams" % "2.1.6"
libraryDependencies += compilerPlugin(
  "dev.zio" %% "zio-profiling-tagging-plugin" % "0.3.1"
)
//// https://mvnrepository.com/artifact/dev.zio/zio-streams
Compile / mainClass := Some("network.WebSocketAdvanced")
//Compile / run / fork := true
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}
