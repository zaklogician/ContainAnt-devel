package com.containant.casestudies

import com.containant._
import com.containant.heuristics._

object CS6TypeErasure {
  
  //////////////////////////////////////////////////////////////////////
  // Configuration
  val _seed: Int = 0xDEADBEEF
  val _runs: Int = 10
  // target fitness fn. evaluations ~ 100 for comparison with SMAC, which often
  //                                  terminates under 100 on this prob.
  
  object Hmma extends AntHeuristic {
    override val _maxPheromone: Double = 10
    override val _evaporationRate: Double = 0.4
    override val _iterations: Int = 100
    override val _antNumber: Int = 1
    override val _minimumFraction: Double = 0.10
    override val _recursionDepth: Int = 8
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "mma"
  }
  
  object Hgre extends GrEvoHeuristic {
    override val _population: Int = 100
    override val _length: Int = 9
    override val _maxChoice: Int = 7
    override val _tournamentSize = 5
    override val _generations = 10
    override val _recursionDepth = 10
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "gre"
  }

  object Hran extends RandomHeuristic {
    override val _iterations = 100
    override val _recursionDepth = 8
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "ran"
  }

  //////////////////////////////////////////////////////////////////////

  // Problem Description

  case class Iter1(i: String)
  case class Iter2(i: String)
  case class MyGeneric[T2,T](t: T)
  case class Whateva(t: MyGeneric[String,String])
  // case class Whateva(t: MyGeneric[Iter1,Iter2])
  
  type Solution = Whateva
  
  object TEModule extends Module {
    val evil1: MyGeneric[Iter1,Iter1] = MyGeneric(Iter1("1"))
    val evil2: MyGeneric[Iter1,Iter2] = MyGeneric(Iter2("2"))
    
    def whateva(t: MyGeneric[String,String]) = Whateva(t)
    // def whateva(t: MyGeneric[Iter2,Iter2]) = Whateva(t)
  }

  def fitness(solution: Solution): Double = solution.t.t.length

  //////////////////////////////////////////////////////////////////////
  // Experiment Details


  def main(args: Array[String]): Unit = {
    println("\n-----------------------------")
    println("Case Study: Type Erasure")
    println("Runs: " + _runs)
    
    val comparison02 =
      Framework.experiment[Solution](Hran, Hran, _runs, TEModule, fitness _ )
    
    println("heuristic,min,mean,max,var")
    println(comparison02.summary1)
    println(comparison02.summary2)
    println("p: " + comparison02.pvalue)
    println()

    println("Best solution: ")
    println( comparison02.results1.maxBy(_.fitness) )
  }
  
}

// End ///////////////////////////////////////////////////////////////
