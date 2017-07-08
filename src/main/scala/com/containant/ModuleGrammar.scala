package com.containant


class ModuleGrammar(module: Module) extends LBNF {
  // import java.lang.reflect.Method
  import scala.reflect.runtime.universe._
  private val rm = scala.reflect.runtime.currentMirror
  
  private val anyMethods = ( rm.classSymbol(classOf[Any]).toType.members.collect {
      case m: MethodSymbol => m 
    }  ).toList
    
  override type Sort = Type // Class[_]
  override type Label = MethodSymbol // Method
  
  // See http://docs.scala-lang.org/overviews/reflection/symbols-trees-types
  def isAssignableFrom(lhs: Sort, rhs: Sort): Boolean = {
   // lhs <:< rhs
   val lhsClass = rm.runtimeClass(lhs.typeSymbol.asClass)
   val rhsClass = rm.runtimeClass(rhs.typeSymbol.asClass)
   val isAssignable = lhsClass.isAssignableFrom(rhsClass)
   
   val result = rhs =:= lhs || rhs <:< lhs
   if( result != isAssignable )
    println( s"isAssignable: ${isAssignable} $result from $lhs $rhs $lhsClass $rhsClass" )
   // lhs.getClass.isAssignableFrom( rhs.getClass )
   result
  }
  
  /***
  private val providers: List[Method] = {
    module.getClass.getMethods.toList.sortBy(_.getName).filter { m => 
      !List( "equals","hashCode","toString", "getClass",
            "wait", "notify", "notifyAll"
           ).contains(m.getName)
    }
  }
  ***/
  
  private val providers: List[MethodSymbol] = {
  
    val methods = rm.classSymbol(module.getClass).toType.members.collect {
          case m: MethodSymbol if m.isPublic && !anyMethods.contains(m) && !m.isConstructor => m 
    }

//println(s"$methods, ${methods.size}")
//    for(m <- methods ) {
//      println(s"$module: ${instanceMirror.reflectMethod(m).apply()}")
//    }
    methods.toList
  }
  
  override def sort(label: Label): Sort =
    label.returnType // label.getReturnType

  override val sorts: Seq[Sort] =
    providers.map(sort).distinct

//println( s"sorts: $sorts" )          
  
  override def labels(sort: Sort): Seq[Label] =
    providers.filter { p => isAssignableFrom(sort,p.returnType) }
  
  override def rule(label: Label): Seq[Sort] = {
//println( s"rule: $label ${label.paramLists} ")         

    label.paramLists.flatten.map { _.info } // label.getParameterTypes
  }
    
  def construct(tree: SyntaxTree): Object = {
//println( "construct tree.label: " + tree.label )         
    val arguments = tree.subtrees.map(construct)
    // tree.label.invoke(module, arguments:_*)
//println( "construct arguments " + arguments.mkString( "," ) )   

    val instanceMirror = rm.reflect(module)        
    instanceMirror.reflectMethod(tree.label).apply(arguments:_*).asInstanceOf[Object]
  }
}
