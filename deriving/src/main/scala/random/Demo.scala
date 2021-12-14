package random

enum SiteMember:
  case RegisteredUser(id: Long, email: String, isAdmin: Boolean)
  case AnonymousUser(session: String)

given randStr: Random[String] with
  def generate(): String = scala.util.Random.nextString(5)

given randLong: Random[Long] with
  def generate(): Long = scala.util.Random.nextLong()

given randBool: Random[Boolean] with
  def generate(): Boolean = scala.util.Random.nextBoolean()

@main def randomDemo(): Unit =
  println(summon[Random[SiteMember]].generate())
  println(summon[Random[SiteMember]].generate())
  println(summon[Random[SiteMember]].generate())

