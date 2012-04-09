package org.cb.dslconverter.plugin

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._
import java.util.concurrent.atomic.AtomicLong

class DslConverterPlugin(val global: Global) extends Plugin {
  // import the globals classes - since we use them often
  import global._
  import definitions._

  // The name of the annotation class
  val DSL_CONVERTER_CLASS_NAME: String = "org.cb.dslconverter.annotations.DslConvertable"

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

    // Still magic to me
    val global: DslConverterPlugin.this.global.type = DslConverterPlugin.this.global

    // when should this PluginComponent be ran
    val runsAfter = List[String]("typer")

    // What name should we give the phase for this plugin
    val phaseName = DslConverterPlugin.this.name

    // In the case of also extending transform, we need a Transformer Class
    def newTransformer(unit: global.CompilationUnit) = DslConverterTransformer

    // Implementation of the Transformer class
    object DslConverterTransformer extends Transformer {

      // main method for transforming the tree
      override def transform(tree: global.Tree) = {

        // lets go through the tree
        val newTree = tree match {

          // the case of a Class Definition (global.ClassDef)
          case cd @ ClassDef(modifiers, className, params, impl) => { cd }
          case pd @ PackageDef(packageName, stats) => {
            // we have a package - lets check for classes that are annotated  
            val newStats = stats.flatMap {
              // for each of the annotated classes 
              case cd @ ClassDef(modifiers, className, tparams, impl) if annotatedWithDslConverter(cd.symbol) => {
                // add the members to include
                val targetMembers = cd.impl.body.filter(shouldMemberBeIncluded_?)
                // create a builder class for it
                val builderClass = generateBuilderClass(pd.symbol, cd.symbol, targetMembers)
                // add the builder class to our list of classes
                cd :: builderClass :: Nil

                // XXX - keep until generateBuilderClass is correct
                cd :: Nil
              }
              // if it's not a ClassDef we want, just move on
              case x => x :: Nil
            }
            // add in the new class into the current package
            treeCopy.PackageDef(pd, packageName, newStats)
          }

          // The case for a Module Definition (global.ModuleDef) 
          case md @ ModuleDef(mods, name, impl) => { md }
          // catch the rest
          case _ => tree
        }

        super.transform(newTree)

      }
    }
  }

  val sequence = new AtomicLong(0)

  /**
   * Generates a Builder class as a child of the packageClass, matching the name and types of caseClass
   */
  def generateBuilderClass(packageClass: Symbol, caseClass: Symbol, caseClassMembers: List[Tree]): ClassDef = {
    // create the name
    val caseClassBuilderName: TypeName = caseClass.name.append("Builder").toTypeName
    // create the ClassSymbol for our builder class
    val caseClassBuilder: ClassSymbol = packageClass.newClass(packageClass.pos.focus, caseClassBuilderName)
    // look for the non optional fields
    val mandatoryFields = caseClassMembers.filter(member => {
      !(member.asInstanceOf[DefDef]).tpt.toString().contains("Option")
    })
    // now that we have the mandatory fields, create types for each of the fields
    // for example val time: Double becomes the type [HAS_TIME]
    val builderMandatoryParamTypes: List[Symbol] = {
      mandatoryFields.map(field => {
        val fieldName = (field.asInstanceOf[DefDef]).name
        val param = caseClassBuilder.newTypeParameter(caseClassBuilder.pos.focus, newTypeName("HAS_").append(fieldName.toString().toUpperCase()))
        param setFlag (DEFERRED | PARAM)
        param
      })
    }

    // Does this add in the types to the new class?
    caseClassBuilder setInfo polyType(
      builderMandatoryParamTypes.map(_.tpe.typeSymbol),
      ClassInfoType(
        ObjectClass.tpe :: ScalaObjectClass.tpe :: Nil,
        new Scope,
        caseClassBuilder))
    
    

    println(caseClassBuilder.info)

    null
  }

  /**
   * Check to see if a given Symbol is annotated with our @DslConverter annotation
   */
  private def annotatedWithDslConverter(sym: Symbol) = sym.isCaseClass && sym.annotations.exists(_.atp.toLongString == DSL_CONVERTER_CLASS_NAME)
}
