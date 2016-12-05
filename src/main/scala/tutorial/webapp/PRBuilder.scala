package PRBuilder

object State {

  case class ActiveRepos(names: List[String])

  case class State(activeRepos: ActiveRepos)

}

object Data {
  var reposFixture = List(new Repo("repo1"), new Repo("hello world"), new Repo("mySexyRepo"))
  val buildDetails = List(
    BuildDetails(true, 1000, List("this is a test, it suceeded")),
    BuildDetails(false, 2000, List("this is a test, it failed")),
    BuildDetails(true, 3000, List("this is a test, it failed", "this is a test, it failed"))
  )

  val buildsFixture = buildDetails.map(bd => Build(math.random().toString(), "build_name", "https://google.com", bd))
  reposFixture = reposFixture.map(r => {
    buildsFixture.foreach(build => {
      r.addBuild(build)
    })
    r
  })

  type RepoName = String

  case class BuildDetails(success: Boolean, created: Int, testOutput: List[String])
  case class Build(id: String, name: String, link: String, details: BuildDetails)
  case class Repos(repos: List[Repo])

  trait BaseRepo {
    var builds: Builds
    type Builds = List[Build]
    def updateBuilds(newBuild: Build): Unit
    def addBuild(newBuild: Build): Unit
  }

  class Repo(repoName: String) extends BaseRepo {
    val name = repoName
    var builds = List[Build]()
    def updateBuilds(newBuild: Build): Unit = {
      builds = builds.map(build => {
        if (build.id == newBuild.id) newBuild
        else build
      })
    }

    def addBuild(newBuild: Build): Unit = {
      builds = newBuild :: builds
    }
  }

}