package random

import scala.util.Random as ScalaRandom

enum SiteMember:
  case RegisteredUser(id: Long, email: String, isAdmin: Boolean)
  case AnonymousUser(session: String)

given randStr: Random[String] with
  def generate(): String = ScalaRandom.alphanumeric.take(5).mkString

given randLong: Random[Long] with
  def generate(): Long = ScalaRandom.nextLong(1000000)

given randBool: Random[Boolean] with
  def generate(): Boolean = ScalaRandom.nextBoolean()

@main def randomDemo(): Unit =
  println(summon[Random[SiteMember]].generate())
  println(summon[Random[SiteMember]].generate())
  println(summon[Random[SiteMember]].generate())

