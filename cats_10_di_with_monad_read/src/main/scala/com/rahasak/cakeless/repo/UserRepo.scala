package com.rahasak.cakeless.repo

import com.rahasak.cakeless.protocol.User

trait UserRepo {

  def create(user: User): Long

  def get(id: Long): User

}
