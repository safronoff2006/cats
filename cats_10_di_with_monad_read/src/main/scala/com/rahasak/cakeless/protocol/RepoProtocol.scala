package com.rahasak.cakeless.protocol

import com.rahasak.cakeless.repo.{PermissionRepo, UserRepo}

case class Repo(userRepo: UserRepo, permRepo: PermissionRepo)

