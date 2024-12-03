lazy val root = project.in(file("."))
    .settings(
        name := "AdventOfCode2024",
        version := "0.1",
        scalaVersion := "3.3.3",
        libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1",
    )

