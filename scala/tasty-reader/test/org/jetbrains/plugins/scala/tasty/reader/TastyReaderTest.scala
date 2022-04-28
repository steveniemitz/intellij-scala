package org.jetbrains.plugins.scala.tasty.reader

import scala.util.control.NonFatal
//import org.junit.Assert

import java.io.File
import java.nio.file.{FileSystems, Files, Path}
import scala.collection.mutable

// test quotes in textOfType, given, extension, package, qualifier (plus format)
// enum companion: case(), object
// Target names
// Escape chars
// Compare: single dir
// type MirroredElemTypes = EmptyTuple, type MirroredElemTypes = scala.Tuple$package.EmptyTuple
// Anonymous Context Parameters?
// infix types
// abstract extension
// hkt as arg, lambda
// match types
// super
// annotation: parameter, type, string, array
// TODO Nothing -> Any when for parameter (variance)
// TODO type trees
// TODO different name kinds, FQN
// TODO symbol names `foo`
// TODO val a, b; val (a, b)
// TODO transparent inline def quotes in the same file
//
// TODO refactor code
// TODO StringBuilder
// TODO children lazy loading
// TODO Use Scala 2 (either read or copy tasty-core)
// TODO exhaustive matches
// TODO getOrElse(throw exception)
// TODO gzip
//
// TODO rely on signed name instead of Apply template parent calls?
// TODO FunctionN, TupleN
// TODO infix types (not just & and |)
// TODO self type
// TODO abstract override (order)
// TODO = derived ?
// TODO modifiers order
// TODO option to skip private definitions
// TODO detect anonymous givens more reliably?
// TODO how to merge object / implicit class / enum members, index?
// TODO re-elaborate context bounds?
// TODO package objects as package objects?
// TODO default argument constants?
// TODO group enum cases
// TODO group extension methods
// TODO combinedUsingClauses?
// TODO use Unit method result instead of Int
// TODO use objects instead of traits?
// TODO benchmark
// TODO correspondence between parametric type definitions and type lambdas - which to use?
//
// TODO method to parse a JAR, compare results, benchmark
// TODO Convert to unit tests (depends on https://youtrack.jetbrains.com/issue/SCL-19023)
// TODO Convert ./data to test data
object TastyReaderTest {

  def main(args: Array[String]): Unit = {
    var passed, failed = Seq.empty[String]

    Seq(
      "annotation/Members",
      "annotation/Multiple",
      "annotation/Parameters",
      "annotation/Text",
      "member/Bounds",
      "member/Def",
      "member/ExtensionMethod",
      "member/Given",
      "member/Identifiers",
      "member/InlineModifier",
      "member/Modifiers",
      "member/Qualifier",
      "member/This",
      "member/Type",
      "member/Val",
      "member/Var",
      "package1/package2/package",
      "package1/package2/Nested",
      "package1/package2/NestedImport",
      "package1/package2/Prefix",
      "package1/package2/Scope",
      "package1/Members",
      "package1/topLevel",
      "parameter/Bounds",
      "parameter/ByName",
      "parameter/CaseClass",
      "parameter/Class",
      "parameter/ContextBounds",
      "parameter/Def",
      "parameter/DefaultArguments",
      "parameter/Enum",
      "parameter/EnumCaseClass",
      "parameter/Extension",
      "parameter/ExtensionMethod",
      "parameter/Given",
      "parameter/HKT",
      "parameter/HKTBounds",
      "parameter/HKTVariance",
      "parameter/Identifiers",
      "parameter/InlineModifier",
      "parameter/Modifiers",
      "parameter/Qualifier",
      "parameter/Repeated",
      "parameter/Trait",
      "parameter/Type",
      "parameter/Variance", // TODO TypeMember
      "typeDefinition/Class",
      "typeDefinition/Companions",
      "typeDefinition/Enum",
      "typeDefinition/Identifiers",
      "typeDefinition/ImplicitClass",
      "typeDefinition/Members",
      "typeDefinition/Modifiers",
      "typeDefinition/Object",
      "typeDefinition/Parents",
      "typeDefinition/Qualifier",
      "typeDefinition/Trait",
      "types/And",
      "types/Annotated",
      "types/Constant",
      "types/Function",
      "types/FunctionContext",
      "types/FunctionPolymorphic",
      "types/Ident",
      "types/Lambda",
      "types/Literal",
      "types/Or",
      "types/Projection",
      "types/Refinement",
      "types/Refs",
      "types/Select",
      "types/Singleton",
      "types/This",
      "types/Tuple",
      "types/Wildcard",
      "EmptyPackage",
      "Nesting",
    ).map("community/scala/tasty-reader/testdata/" + _ + ".scala").foreach { scalaFile =>
      assertExists(scalaFile)

      val tastyFile = {
        val packageFile = scalaFile.replaceFirst("\\.scala", "\\$package.tasty")
        if (exists(packageFile)) packageFile else scalaFile.replaceFirst("\\.scala", ".tasty")
      }
      assertExists(tastyFile)

      val tree = TreeReader.treeFrom(readBytes(tastyFile))

      val (sourceFile, actual) = try {
        val treePrinter = new TreePrinter(privateMembers = true)
        treePrinter.fileAndTextOf(tree)
      } catch {
        case NonFatal(e) =>
          Console.err.println(scalaFile)
          throw e
      }

      assert(sourceFile == File(scalaFile).getName, s"File: $scalaFile, read: $sourceFile")

      val expected = new String(readBytes(scalaFile))
        .replaceAll(raw"(?s)/\*\*/.*?/\*(.*?)\*/", "$1")

      val actualFile = scalaFile.replaceFirst("\\.scala", ".actual")

      if (actual != expected) {
        println(scalaFile)
        println("---Actual---")
        println(actual)
        println("---Expected---")
        println(expected)
        println("---")
        println("")
        Files.write(Path.of(actualFile), actual.getBytes)
        failed :+= scalaFile
      } else {
        if (exists(actualFile)) {
          Files.delete(Path.of(actualFile))
        }
        passed :+= scalaFile
      }
    }
    if (failed.isEmpty) println(s"Tests passed: ${passed.length}")
    else Console.err.println(s"Tests passed: ${passed.length}, failed: ${failed.length}:\n" + failed.map("  " + _).mkString("\n"))
  }

  private def assertExists(path: String): Unit = assert(exists(path), path)

  private def exists(path: String): Boolean = new File(path).exists()

  private def readBytes(file: String): Array[Byte] = Files.readAllBytes(Path.of(file))
}
