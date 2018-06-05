scalaVersion := "2.12.0"

fork in run := true
cancelable in Global := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
