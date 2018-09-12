import Dependencies._

// POM settings for Sonatype
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization      := "com.github.ardexz",
      homepage          := Some(url("https://github.com/rexardiente/ripple-transaction-parser")),
      developers        := List(Developer("com.github.rexardiente","Rex Ardiente","rexardiente09@gmail.com",url("https://github.com/rexardiente"))),
      scalaVersion      := "2.11.7",
    )),
    version             := "0.1.0",      
    pgpReadOnly         := false,
    publishMavenStyle   := true,
    name                := "Ripple Transaction Parser"
  )

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
libraryDependencies ++= Seq(
  "com.typesafe.play"   %% "play-json"           % "2.6.9",
  "org.joda"            %  "joda-convert"        % "2.1.1",
  scalaTest             %  Test
)

// Add sonatype repository settings
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)
