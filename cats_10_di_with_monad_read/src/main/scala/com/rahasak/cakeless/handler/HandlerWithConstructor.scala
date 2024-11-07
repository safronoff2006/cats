package com.rahasak.cakeless.handler

import com.rahasak.cakeless.protocol.{Permission, Repo, User}

object UserHandlerWithConstructor {

  def createUser(repo: Repo, user: User): Long = {
    repo.userRepo.create(user)
  }

  def getUser(repo: Repo, id: Long): User = {
    repo.userRepo.get(id)
  }

}

object PermissionHandlerWithConstructor {

  def createPermission(repo: Repo, permission: Permission): Long = {
    repo.permRepo.create(permission)
  }

  def getPermission(repo: Repo, id: Long): Permission = {
    repo.permRepo.get(id)
  }

  def searchPermission(repo: Repo, role: String): List[Permission] = {
    repo.permRepo.search(role)
  }

}
