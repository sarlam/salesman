package me.jarleton.voy

import me.jarleton.voy.Genetic._

import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import scala.util.Random.shuffle

/**
  * Created by paul on 07/02/17.
  */
class Genetic {
  val conf = loadConf()
  val n = conf.getOrElse("n", "0").toInt
  val costMatrix = mkCostMatrix(conf.getOrElse("pathToGraph", "graph"), n)
  val alphabet = conf.getOrElse("alphabet", "ABCDEF").toList
  val maxIteration = conf.getOrElse("maxIteration", "10").toInt
  val mutateRate = conf.getOrElse("mutateRate", "0.03").toDouble
  val populationSize = conf.getOrElse("populationSize", "100").toInt
  var population = Array.fill[Traveller](populationSize)(generateTraveller)
  val mutateNumber = (populationSize * mutateRate).toInt

  var currentIteration = 0


  while (currentIteration != maxIteration) {
    sortPop()
    crossover()
    mutate()
    currentIteration += 1
  }

  def getResult: Traveller = {
    sortPop()
    population(0)
  }

  def sortPop() = {
    population = population.sortBy(_.cost)
  }

  def mutate() = {
    val mutateTravellerIndex = Seq.fill[Int](mutateNumber)(Random.nextInt(populationSize - 1) % populationSize)
    for (i <- mutateTravellerIndex) {
      insertToPopulation(i, insertMutation(population(i).path))
    }
  }

  def crossover() = {
    for (i <- 2 to 30 by 2) {
      val child = reproduction(population(i).path, population(i + 1).path, n)
      insertToPopulation(populationSize - i, child)
    }
  }

  def fitness(chemin: String): Int = {
    var cost: Int = 0
    for (i <- Range(0, chemin.length - 1)) {
      cost = cost + costMatrix(convert(chemin(i)))(convert(chemin(i + 1)))
    }
    cost + costMatrix(convert(chemin(0)))(convert(chemin(chemin.length - 1)))
  }

  def generateTraveller: Traveller = {
    val chemin: String = shuffle(alphabet).mkString
    Traveller(chemin, fitness(chemin))
  }

  def insertToPopulation(key: Int, individu: String): Unit = {
    population(key) = Traveller(individu, fitness(individu))
  }

  def loadConf(): mutable.Map[String, String] = {
    val conf = "conf"
    val finalConf: mutable.Map[String, String] = mutable.Map[String, String]()

    for (line <- Source.fromResource(conf).getLines().toList) {
      val option = line.split(" ")

      finalConf += option(0) -> option(1)
    }
    finalConf
  }

  def mkCostMatrix(pathToGraph: String, n: Int): Array[Array[Int]] = {
    val conf = Source.fromResource(pathToGraph).getLines().toList
    val returnedArray = Array.ofDim[Int](n, n)
    for (line <- conf) {
      val costPath = Array[String](line.slice(0, 1), line.slice(1, 2), line.slice(2, line.length))
      val A = convert(costPath(0))
      val B = convert(costPath(1))
      val cost = convert(costPath(2))
      returnedArray(A)(B) = cost
      returnedArray(B)(A) = cost
    }
    returnedArray
  }
}

object Genetic {

  def reproduction(p1: String, p2: String, n: Int): String = {
    val child: Array[Char] = Array.fill[Char](n)('*')
    var index = 0

    while (child(index) == '*') {
      child(index) = p1(index)
      index = p2.indexOf(p1(index))
    }

    // ugly !
    for (i <- Range(0, n)) {
      if (child(i) == '*') {
        child(i) = p2(i)
      }
    }
    child.mkString
  }

  def insertMutation(individu: String): String = {
    val ind = individu.toArray
    val r1 = Random.nextInt(100) % ind.length
    var r2 = -1
    while (r2 < 0 || r2 == r1) {
      r2 = Random.nextInt(100) % ind.length
    }
    val aux = ind(r1)
    ind(r1) = ind(r2)
    ind(r2) = aux
    ind.mkString
  }

  def convert(c: String): Int = c match {
    case "A" => 0
    case "B" => 1
    case "C" => 2
    case "D" => 3
    case "E" => 4
    case "F" => 5
    case "G" => 6
    case "H" => 7
    case "I" => 8
    case "J" => 9
    case "K" => 10
    case "L" => 11
    case "M" => 12
    case "N" => 13
    case "O" => 14
    case "P" => 15
    case "Q" => 16
    case "R" => 17
    case "S" => 18
    case "T" => 19
    case "U" => 20
    case "V" => 21
    case "W" => 22
    case "X" => 23
    case "Y" => 24
    case "Z" => 25
    case _ => c.toInt
  }

  def convert(c: Char): Int = c match {
    case 'A' => 0
    case 'B' => 1
    case 'C' => 2
    case 'D' => 3
    case 'E' => 4
    case 'F' => 5
    case 'G' => 6
    case 'H' => 7
    case 'I' => 8
    case 'J' => 9
    case 'K' => 10
    case 'L' => 11
    case 'M' => 12
    case 'N' => 13
    case 'O' => 14
    case 'P' => 15
    case 'Q' => 16
    case 'R' => 17
    case 'S' => 18
    case 'T' => 19
    case 'U' => 20
    case 'V' => 21
    case 'W' => 22
    case 'X' => 23
    case 'Y' => 24
    case 'Z' => 25
    case _ => c.toInt
  }
}
