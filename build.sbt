name := "scala-drl"

version := "0.9"

scalaVersion := "2.11.7"

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

classpathTypes += "maven-plugin"

libraryDependencies ++= Seq(
  "org.deeplearning4j" % "deeplearning4j-core" % "0.4-rc3.10"
 , "org.nd4j" % "nd4j-native" % "0.4-rc3.10"
 , "org.nd4j" % "nd4j-native" % "0.4-rc3.10" classifier "linux-x86_64"
 , "com.github.wookietreiber" %% "scala-chart" % "0.5.0"
 , "org.bytedeco" % "javacpp" % "1.2-SNAPSHOT"
 , "com.itextpdf" % "itextpdf" % "5.5.6"
)

javaOptions in run += s"""-Djava.library.path="""""
