// Comment to get more information during initialization
// logLevel := Level.Warn

//http://stackoverflow.com/a/24357648
resolvers ++= Seq(
  "Local Maven Repository" at "" + Path.userHome.asFile.toURI.toURL + ".m2/repository",
  DefaultMavenRepository,
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
  Resolver.url("Play", url("http://download.playframework.org/ivy-releases/"))(Resolver.ivyStylePatterns)
)

resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

