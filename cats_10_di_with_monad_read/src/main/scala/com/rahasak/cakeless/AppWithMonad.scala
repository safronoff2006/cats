package com.rahasak.cakeless

import cats.Id
import com.rahasak.cakeless.handler.{PermissionHandlerWithMonad, UserHandlerWithMonad, UserPermissionHandlerWithMonad}
import com.rahasak.cakeless.protocol.{Permission, Repo, User}
import com.rahasak.cakeless.repo.{PermissionRepoImpl, UserRepoImpl}

object AppWithMonad extends App {

  // dependencies
  val userRepo: UserRepoImpl = new UserRepoImpl
  val permRepo: PermissionRepoImpl = new PermissionRepoImpl
  val repo: Repo = Repo(userRepo, permRepo)

  // create user
  val createUserResp: Id[Long] = UserHandlerWithMonad.createUser(User(1001, "lambda", "admin")).run(repo)
  println(createUserResp)

  // output
  // 1001


  // get user
  val getUserResp: Id[User] = UserHandlerWithMonad.getUser(1001).run(repo)
  println(getUserResp)

  // output
  // User(1001,lambda1001,admin)


  // create permission
  val createPermResp: Id[Long] = PermissionHandlerWithMonad.createPermission(Permission(2001, "admin", "use_auth")).run(repo)
  println(createPermResp)

  // output
  // 2001


  // get permission
  val getPermResp: Id[Permission] = PermissionHandlerWithMonad.getPermission(1001).run(repo)
  println(getPermResp)

  // output
  // Permission(1001,legal_officer,use_archive)


  // get permissions of user
  val permissions: Id[List[Permission]] = UserPermissionHandlerWithMonad.getUserPermissions(1001).run(repo)
  println(permissions)

  // output
  // List(Permission(1001,user,user_archive), Permission(1002,user,use_doc_storage), Permission(1005,user,use_auth))

}
