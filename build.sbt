Global / scalaVersion := "3.3.1"
ThisProject / organization := "mlanin"
ThisProject / name := "aoc2023"

lazy val root = project
  .aggregate(puzzles)

lazy val puzzles = project
  .settings(
    // The following forking and env settings make running apps consistent between
    // sbt and IntelliJ IDEA in bsp mode.
    // Current bsp integration in IDEA silently overrides working directory and env vars,
    // that were selected in the Run dialog, with baseDirectory.value and envVars.value,
    // received from sbt. Since overriding baseDirectory is not an option due to its impact
    // we pass the inputs root directory from the env instead, making the app agnostic to
    // its working directory.
    fork := true,
    envVars := envVars.value ++ List(
      "AOC_INPUTS" -> s"${(LocalRootProject / baseDirectory).value / "inputs"}",
    ),
    libraryDependencies ++= List(
      "com.github.scopt" %% "scopt"           % "4.1.0",
      "com.lihaoyi"      %% "fastparse"       % "3.0.2",
      "ch.qos.logback"    % "logback-classic" % "1.4.7",
      "io.getkyo"        %% "kyo-core"        % "0.7.0",
      "io.getkyo"        %% "kyo-direct"      % "0.7.0",
    ),
  )
