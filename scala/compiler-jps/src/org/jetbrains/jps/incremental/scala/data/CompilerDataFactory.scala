package org.jetbrains.jps.incremental.scala.data

import java.io.File
import java.util

import org.jetbrains.jps.ModuleChunk
import org.jetbrains.jps.incremental.CompileContext
import org.jetbrains.jps.incremental.java.JavaBuilder
import org.jetbrains.jps.incremental.scala.{ScalaBuilder, SettingsManager}
import org.jetbrains.jps.incremental.scala.model.{CompilerSettings, LibrarySettings}
import org.jetbrains.jps.incremental.scala.compilerVersionIn
import org.jetbrains.jps.model.JpsModel
import org.jetbrains.jps.model.java.compiler.JpsJavaCompilerOptions
import org.jetbrains.jps.model.java.{JpsJavaExtensionService, JpsJavaSdkType}
import org.jetbrains.jps.model.library.JpsLibrary
import org.jetbrains.jps.model.module.JpsModule
import org.jetbrains.jps.model.serialization.JpsModelSerializationDataService
import org.jetbrains.plugins.scala.compiler.data
import org.jetbrains.plugins.scala.compiler.data.CompilerJarsFactory.CompilerJarsResolveError
import org.jetbrains.plugins.scala.compiler.data.{CompilerData, CompilerJars, CompilerJarsFactory}
import org.jetbrains.plugins.scala.util.JarUtil
import org.jetbrains.plugins.scala.util.JarUtil.JarFileWithName

import scala.jdk.CollectionConverters._

trait CompilerDataFactory {

  def from(context: CompileContext,
           chunk: ModuleChunk): Either[String, CompilerData]
}

object CompilerDataFactory
  extends CompilerDataFactory {

  override def from(context: CompileContext, chunk: ModuleChunk): Either[String, CompilerData] = {
    val module = chunk.representativeTarget.getModule

    val scalaSdkOpt = SettingsManager.getScalaSdk(module)

    val compilerJars: Either[String, Option[CompilerJars]] =
      scalaSdkOpt
        .map(extractCompilerJars(_, module).map(Some(_)))
        .getOrElse(Right(None))

    val descriptor = context.getProjectDescriptor
    for {
      jars <- compilerJars
      home <- javaHome(descriptor.getModel, module, ScalaBuilder.isCompileServerEnabled(context))
    } yield {
      val incrementality = SettingsManager.getProjectSettings(descriptor.getProject).getIncrementalityType
      data.CompilerData(jars, home, incrementality)
    }
  }

  private def extractCompilerJars(scalaSdk: JpsLibrary, module: JpsModule): Either[String, CompilerJars] =
    compilerJarsInSdk(scalaSdk)
      .flatMap(validateAllFilesExist)
      .left.map(toErrorMessage(_, scalaSdk, module))

  private def validateAllFilesExist(jars: CompilerJars): Either[CompilerJarsResolveError.FilesDoNotExist, CompilerJars] = {
    val absentJars = jars.allJars.filterNot(_.exists)
    Either.cond(
      absentJars.isEmpty,
      jars,
      CompilerJarsResolveError.FilesDoNotExist(absentJars)
    )
  }

  private def javaHome(model: JpsModel,
                       module: JpsModule,
                       isCompileServerEnabled: Boolean): Either[String, Option[File]] = {
    val jdkOpt = Option(module.getSdk(JpsJavaSdkType.INSTANCE))
    jdkOpt
      .toRight("No JDK in module " + module.getName)
      .flatMap { moduleJdk =>

        val jvmSdk = if (isCompileServerEnabled) {
          val global = model.getGlobal
          Option(SettingsManager.getGlobalSettings(global).getCompileServerSdk).flatMap { sdkName =>
            global.getLibraryCollection
              .getLibraries(JpsJavaSdkType.INSTANCE)
              .asScala
              .find(_.getName == sdkName)
          }
        } else {
          Option(model.getProject.getSdkReferencesTable.getSdkReference(JpsJavaSdkType.INSTANCE))
            .flatMap(references => Option(references.resolve))
        }

        if (jvmSdk.map(_.getProperties).contains(moduleJdk)) {
          Right(None)
        } else {
          val directory = new File(moduleJdk.getHomePath)
          Either.cond(directory.exists, Some(directory), "JDK home directory does not exists: " + directory)
        }
      }
  }

  def hasScala3(modules: Set[JpsModule]): Boolean =
    modules.exists {
      compilerJarsIn(_).exists(_.hasScala3)
    }

  private def hasOldScala(modules: Set[JpsModule]) =
    hasVersions(modules, "2.8", "2.9")

  //noinspection SameParameterValue
  private def hasVersions(modules: Set[JpsModule], versions: String*) =
    modules.exists {
      compilerJarsIn(_).forall { cj =>
        compilerVersionIn(cj.compilerJar, versions: _*)
      }
    }

  def scalaOptionsFor(compilerSettings: CompilerSettings, chunk: ModuleChunk): Seq[String] = {
    val modules = chunk.getModules.asScala.toSet
    val hasScala3 = CompilerDataFactory.hasScala3(modules)
    val configuredOptions = compilerSettings.getCompilerOptionsAsStrings(hasScala3)

    val bootOptions = bootClasspathOptions(hasOldScala(modules))
    val semanticDBOptions = semanticDbOptionsFor(configuredOptions.toIndexedSeq, chunk)
    bootOptions ++ semanticDBOptions ++ configuredOptions
  }

  private def bootClasspathOptions(hasOldScala: Boolean): Seq[String] =
    if (hasOldScala) {
      Seq("-nobootcp", "-javabootclasspath", File.pathSeparator)
    } else {
      Seq.empty
    }

  // https://youtrack.jetbrains.com/issue/SCL-17519
  private def semanticDbOptionsFor(configuredOptions: Seq[String], chunk: ModuleChunk): Seq[String] = {
    val hasSemanticDbPlugin = configuredOptions.exists(s => s.startsWith("-Xplugin:") && s.contains("semanticdb-scalac") && new File(s.substring(9)).exists())
    val hasSourceRootOption = configuredOptions.exists(_.startsWith("-P:semanticdb:sourceroot:"))

    if (hasSemanticDbPlugin && !hasSourceRootOption) {
      val project = chunk.representativeTarget.getModule.getProject
      val baseDirectory = JpsModelSerializationDataService.getBaseDirectory(project).getAbsolutePath
      Seq(s"-P:semanticdb:sourceroot:$baseDirectory")
    } else {
      Seq.empty
    }
  }

  def javaOptionsFor(context: CompileContext, chunk: ModuleChunk): Seq[String] = {
    val compilerConfig = {
      val project = context.getProjectDescriptor.getProject
      JpsJavaExtensionService.getInstance.getCompilerConfiguration(project)
    }

    val options = new util.ArrayList[String]()

    addCommonJavacOptions(options, compilerConfig.getCurrentCompilerOptions)

    val annotationProcessingProfile = {
      val module = chunk.representativeTarget.getModule
      compilerConfig.getAnnotationProcessingProfile(module)
    }

    JavaBuilder.addCompilationOptions(options, context, chunk, annotationProcessingProfile)

    options.asScala.toSeq
  }

  // TODO JavaBuilder.loadCommonJavacOptions should be public
  private def addCommonJavacOptions(options: util.ArrayList[String], compilerOptions: JpsJavaCompilerOptions): Unit = {
    if (compilerOptions.DEBUGGING_INFO) {
      options.add("-g")
    }

    if (compilerOptions.DEPRECATION) {
      options.add("-deprecation")
    }

    if (compilerOptions.GENERATE_NO_WARNINGS) {
      options.add("-nowarn")
    }

    if (compilerOptions.ADDITIONAL_OPTIONS_STRING.nonEmpty) {
      // TODO extract VM options
      options.addAll(compilerOptions.ADDITIONAL_OPTIONS_STRING.split("\\s+").toSeq.asJava)
    }
  }

  private def compilerJarsIn(module: JpsModule): Option[CompilerJars] = {
    val sdk = SettingsManager.getScalaSdk(module)
    sdk.flatMap(compilerJarsInSdk(_).toOption)
  }

  private def compilerJarsInSdk(sdk: JpsLibrary): Either[CompilerJarsResolveError, CompilerJars] = {
    val files = compilerClasspath(sdk)
    val jarFiles = JarUtil.collectJars(files)
    CompilerJarsFactory.fromJarFiles(jarFiles)
  }

  private def compilerClasspath(sdk: JpsLibrary): Seq[File] =
    sdk.getProperties match {
      case settings: LibrarySettings => settings.getCompilerClasspath.toSeq
      case _                         => Seq.empty
    }

  private def toErrorMessage(error: CompilerJarsResolveError, scalaSdk: JpsLibrary, module: JpsModule): String = {
    import CompilerJarsResolveError._

    def inScalaCompiler = s"in Scala compiler classpath in Scala SDK ${scalaSdk.getName}"
    def filesNames(files: Iterable[JarFileWithName]) = files.map(_.name).mkString(", ")
    def filePaths(absentJars: Iterable[File]) = absentJars.map(_.getPath).mkString(", ")

    import JarUtil.JarExtension

    error match {
      case NotFound(kind)               => s"No '$kind*$JarExtension' $inScalaCompiler"
      case DuplicatesFound(kind, files) => s"Multiple '$kind*$JarExtension' files (${filesNames(files)}) $inScalaCompiler"
      case FilesDoNotExist(absentJars)  => s"Scala compiler JARs not found (module '${module.getName}'): ${filePaths(absentJars)}"
    }
  }
}
