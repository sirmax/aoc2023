Global / scalaVersion := "3.3.1"
ThisProject / organization := "mlanin"
ThisProject / name := "aoc2023"

lazy val root = project
  .aggregate(puzzles)

lazy val puzzles = project
  .settings(
    libraryDependencies ++= List(
      "com.github.scopt" %% "scopt"           % "4.1.0",
      "com.lihaoyi"      %% "fastparse"       % "3.0.2",
      "ch.qos.logback"    % "logback-classic" % "1.4.7",
      "io.getkyo"        %% "kyo-core"        % "0.7.0",
      "io.getkyo"        %% "kyo-direct"      % "0.7.0",
    ),
  )
