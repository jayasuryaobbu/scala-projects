name := "image_colorizer_application"
 
version := "1.0" 
      
lazy val `image_colorizer_application` = (project in file(".")).enablePlugins(PlayScala, GhpagesPlugin)

git.remoteRepo := "git@github.com:jayasuryaobbu/mr-circuit.github.io.git"

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "https://repo.akka.io/snapshots/"
      
scalaVersion := "2.13.5"

libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice )


      