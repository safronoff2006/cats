package com.rahasak.cakeless.repo

import com.rahasak.cakeless.protocol.Permission

trait PermissionRepo {

  def create(permission: Permission): Long

  def get(id: Long): Permission

  def search(role: String): List[Permission]

}
