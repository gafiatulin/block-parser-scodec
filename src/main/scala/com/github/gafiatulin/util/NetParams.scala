package com.github.gafiatulin.util

trait NetParams{
  def hrp: String
  def pkhPrefix: Array[Byte]
  def shPrefix: Array[Byte]
  def hashFunction: Option[Array[Byte] => Array[Byte]] = None
}