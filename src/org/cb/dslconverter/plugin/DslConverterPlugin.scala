package org.cb.dslconverter.plugin

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global

class DslConverterPlugin(val global: Global) extends Plugin {
  // import the globals classes - since we use them often
  import global._

  // The name of the annotation class
  private val DSL_CONVERTER_CLASS_NAME: String = "org.cb.dslconverter.annotations.DslConvertable"

  // what is the name of our plugin
  val name = "dsl-converter-gen"
  // what does our plugin do
  val description = "generates code to allow for a dsl style syntax to be used with common case classes"
  // what are the components in our plugin
  val components = List[PluginComponent](DslConverterComponent)

  /**
   * Check to see if this tree object is a case accessor, parameter accessor, and a method
   * This means the tree has the details "<caseaccessor> <accessor> <paramaccessor>"
   */
  def shouldMemberBeIncluded_?(tree: Tree) = {
    val sym = tree.symbol
    sym != null && sym.isCaseAccessor && sym.isParamAccessor && sym.isMethod
  }

  /**
   * Plugin Context to be used by our main Plugin
   */
  private object DslConverterComponent extends PluginComponent with Transform {

    // Stil magic to me
    val global: DslConverterPlugin.this.global.type = DslConverterPlugin.this.global
    // when should this PluginComponent be ran
    val runsAfter = List[String]("typer")

    // What name should we give the phase for this plugin
    val phaseName = DslConverterPlugin.this.name

    // In the case of also exending transform, we need a Transformer Class
    def newTransformer(unit: global.CompilationUnit) = DslConverterTransformer

    // Implementation of the Transformer class
    object DslConverterTransformer extends global.Transformer {

      // main method for transforming the tree
      override def transform(tree: global.Tree) = {

        // lets go through the tree
        val newTree = tree match {

          // the case of a Class Definition (global.ClassDef)
          case cd @ ClassDef(modifiers, className, params, impl) => {cd}
          case pd @ PackageDef(packageName, impl) => {
            
            // get the classes that are annotated with our @DslConvertable - and map them to the correct type (ClassDef)
            val classesToMakeBuildersFor: List[ClassDef] = impl.filter( c => annotatedWithDslConverter(c.symbol) ).map( c => c.asInstanceOf[ClassDef])
            
            // get the builder classes created from our annotated classes
            val builderclasses: List[ClassDef] = classesToMakeBuildersFor.map( (c:ClassDef) => {
              // pull out the members we are about from the class
              val targetMembers = c.impl.body.filter(shouldMemberBeIncluded_?)
              // create the builder ClassDefs
              generateBuilderClass(pd.symbol, c.symbol, targetMembers.map(_.symbol)) 
            }) 

            pd
          }
          // The case for a Module Definition (global.ModuleDef), but once again, only if the companion class was annotated 
          case md @ ModuleDef(mods, name, impl) => {md}
          // catch the rest
          case _ => tree
        }

        super.transform(newTree)
      }
    }
  }
  
  /**
   * Generates a Builder class as a child of the packageClass, matching the name and types of caseClass
   */
  def generateBuilderClass(packageClass: Symbol, caseClass: Symbol, caseClassMembers: List[Symbol]): ClassDef = {
    // create the name
    val caseClassBuilderName: TypeName = caseClass.name.append("Builder").toTypeName
    // create the ClassSymbol for our builder class
    val caseClassBuilder: ClassSymbol  = packageClass.newClass(packageClass.pos.focus, caseClassBuilderName)
    
    println(caseClassMembers)
    
    null
  }
  

  /**
   * Check to see if a given Symbol is annotated with our @DslConverter annotation
   */
  private def annotatedWithDslConverter(sym: Symbol) = sym.isCaseClass && sym.annotations.exists(_.atp.toLongString == DSL_CONVERTER_CLASS_NAME)
}