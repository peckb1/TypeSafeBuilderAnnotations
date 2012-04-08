package org.cb.dslconverter.plugin

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global

class DslConverterPlugin(val global: Global) extends Plugin {
  private val DSL_CONVERTER_CLASS_NAME: String = "org.cb.dslconverter.annotations.DslConvertable"

  // what is the name of our plugin
  val name = "dsl-converter-gen"
  // what does our plugin do
  val description = "generates code to allow for a dsl style syntax to be used with common case classes"
  // what are the components in our plugin
  val components = List[PluginComponent](DslConverterComponent)
  
  // keep track of our classes
  val annotatedClasses = collection.mutable.HashMap[global.Symbol, global.ClassDef]()

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

          // the case of a Class Definition (global.ClassDef)              // but only check if it's annotated with what we want
          case cd @ global.ClassDef(modifiers, className, params, typeDef) if(annotatedWithDslConverter(cd)) => {
            // safe off the case class, when we get to its module definition below we need to alter the code
            annotatedClasses += (cd.symbol -> cd)
            // return the original
            cd
          }
          // The case for a Module Definition (global.ModuleDef)
          case md @ global.ModuleDef(mods, name, impl) => {
            println("-----------------------")
            println(md.symbol.tpe)
            println(annotatedClasses)
            println()
//            println(md)
            md
          }
          // if we don't care about it, just return the default
          case _ => tree
        }
        
        super.transform(newTree)
      }
    }
  }
  
  private def annotatedWithDslConverter(cd: global.ClassDef): Boolean = {
    cd.symbol.annotations.exists(anno => {
      anno.atp.toLongString == DSL_CONVERTER_CLASS_NAME
    })
  }
}