enablePlugins(ScalaJSPlugin)

organization in ThisBuild := "com.thoughtworks.binding.experimental"

sonatypeProfileName := "com.thoughtworks.binding"

name := "dom"

crossScalaVersions in ThisBuild := Seq("2.10.7", "2.12.8", "2.11.12")

description := "Reactive web framework for Scala.js."

libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "11.6.0"

libraryDependencies += "com.thoughtworks.sde" %%% "core" % "3.3.1"

libraryDependencies += "com.thoughtworks.extractor" %% "extractor" % "1.2.0"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.5"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test

libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.3"

libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %%% "scala-xml" % "1.1.0")
    case _ =>
      Nil
  }
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

scalacOptions += "-Xexperimental"

scalacOptions ++= {
  if (scalaBinaryVersion.value == "2.10") {
    Nil
  } else {
    Seq("-Ywarn-unused-import")
  }
}

jsDependencies in Test += RuntimeDOM

inConfig(Test) {
  jsEnv in ThisBuild := RhinoJSEnv().value
}
