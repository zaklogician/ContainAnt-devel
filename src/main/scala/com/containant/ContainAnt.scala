package com.containant

import scala.reflect.ClassTag
import com.containant.heuristics._

class ContainAnt(heuristic: Heuristic) {
  
  import scala.reflect.runtime.universe._
  private val rm = scala.reflect.runtime.currentMirror
  
  def create[T](module: Module, optimizeFor: T => Double)(implicit ev: TypeTag[T], ew: ClassTag[T]): T = {
    val grammar = new ModuleGrammar(module)
    def fitness(tree: grammar.SyntaxTree): Double = grammar.construct(tree) match {
      case (t: T) => optimizeFor(t)
      case _      => 0
    }
    val target: grammar.Sort = ev.tpe //rm.classSymbol(ev.runtimeClass).toType // ev.runtimeClass
    //println( s"create: target -- $target" )
    val result: T = heuristic(grammar)( fitness, target ) match {
      case None => throw new Exception("Could not satisfy constraint")
      case Some(x) => grammar.construct(x).asInstanceOf[T]
    }
    result
  }
  
}
