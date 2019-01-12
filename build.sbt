name := "formula"

version := "1.0.0"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-deprecation", "-feature")
classpathTypes += "maven-plugin"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  "org.scalanlp" %% "breeze-viz" % "0.13.2"
)

libraryDependencies ++= Seq(
  "org.nd4j" % "nd4j-native-platform" % "0.7.2",
  "org.slf4j" % "slf4j-api" % "1.7.22",
  "ch.qos.logback" % "logback-classic" % "1.1.8"
)
scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Compile := baseDirectory.value / "src"
