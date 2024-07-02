ThisBuild / scalaVersion := "3.3.1"
// https://mvnrepository.com/artifact/dev.zio/zio
libraryDependencies += "dev.zio" %% "zio" % "2.0.18"
//
//// https://mvnrepository.com/artifact/io.d11/zhttp
//libraryDependencies += "io.d11" %% "zhttp" % "2.0.0-RC1"

// https://mvnrepository.com/artifact/io.d11/zhttp
libraryDependencies += "dev.zio" %% "zio-http" % "3.0.0-RC3"
//libraryDependencies += "com.github.jpbetz" % "subspace" % "0.1.0"
// https://mvnrepository.com/artifact/javax.vecmath/vecmath
libraryDependencies += "javax.vecmath" % "vecmath" % "1.5.2"
// https://mvnrepository.com/artifact/dev.zio/zio-json
libraryDependencies += "dev.zio" %% "zio-json" % "0.5.0"
// https://mvnrepository.com/artifact/org.bouncycastle/bcutil-jdk15on
libraryDependencies += "org.bouncycastle" % "bcutil-jdk15on" % "1.70"
// https://mvnrepository.com/artifact/org.bouncycastle/bcprov-jdk15on
libraryDependencies += "org.bouncycastle" % "bcprov-jdk15on" % "1.70"
// https://mvnrepository.com/artifact/org.bouncycastle/bcpkix-jdk15on
libraryDependencies += "org.bouncycastle" % "bcpkix-jdk15on" % "1.70"

libraryDependencies += "dev.zio" %% "zio-profiling" % "0.3.1"
libraryDependencies += compilerPlugin(
  "dev.zio" %% "zio-profiling-tagging-plugin" % "0.3.1"
)
//// https://mvnrepository.com/artifact/dev.zio/zio-streams
//libraryDependencies += "dev.zio" %% "zio-streams" % "2.0.0-RC2"
Compile / mainClass := Some("network.WebSocketAdvanced")

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}
