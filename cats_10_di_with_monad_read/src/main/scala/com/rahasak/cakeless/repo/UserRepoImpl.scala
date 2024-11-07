package com.rahasak.cakeless.repo

import com.rahasak.cakeless.protocol.User

import scala.util.Random

class UserRepoImpl extends UserRepo {
  override def create(user: User): Long = {
    println(s"user created $user")
    user.id
  }

  override def get(id: Long): User = {
    println(s"get user $id")
    val roles = List("admin", "manager", "legal_officer", "user")
    User(id, s"lambda$id", Random.shuffle(roles).head)
  }
}
