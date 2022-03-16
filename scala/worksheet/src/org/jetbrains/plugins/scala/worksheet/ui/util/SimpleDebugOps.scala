package org.jetbrains.plugins.scala.worksheet.ui.util

trait SimpleDebugOps {
  protected def debug(obj: Any): Unit = {
    val time = System.currentTimeMillis
    val thread = Thread.currentThread.getName
    println(f"[$time%13d] [$thread%-34s] $obj")
  }
}
