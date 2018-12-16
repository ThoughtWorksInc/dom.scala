enablePlugins(Travis)

enablePlugins(SonatypeRelease)


scalacOptions ++= {
  import scala.math.Ordering.Implicits._
  val versionNumers = VersionNumber(scalaVersion.value).numbers
  if (versionNumers >= Seq(2L, 11L) && versionNumers < Seq(2L, 12L)) {
    Seq("-Ybackend:GenBCode")
  } else {
    Seq.empty
  }
}

lazy val secret = project.settings(publishArtifact := false).in {
  val secretDirectory = file(sourcecode.File()).getParentFile / "secret"
  sys.env.get("GITHUB_PERSONAL_ACCESS_TOKEN").foreach { token =>
    IO.delete(secretDirectory)
    org.eclipse.jgit.api.Git
      .cloneRepository()
      .setURI("https://github.com/ThoughtWorksInc/tw-data-china-continuous-delivery-password.git")
      .setDirectory(secretDirectory)
      .setCredentialsProvider(
        new org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider(token, "")
      )
      .call()
      .close()
  }
  secretDirectory
}
