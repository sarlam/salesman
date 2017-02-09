package me.jarleton.voy

/**
  * Created by paul on 07/02/17.
  */
object Main {
  def main(args: Array[String]) {
    println("hello world")

    val geneticSolver = new Genetic()

    println(geneticSolver.getResult)
  }
}