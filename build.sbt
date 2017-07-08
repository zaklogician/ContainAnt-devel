lazy val root = (project in file(".")).
  settings(
    name := "containant",
    version := "0.1.0",
    scalaVersion := "2.11.4",
    mainClass in Compile := Some("com.containant.Main")
  )

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalameta" %% "scalameta" % "1.8.0",
  "org.scala-lang" % "scala-reflect" % "2.11.4"
)
