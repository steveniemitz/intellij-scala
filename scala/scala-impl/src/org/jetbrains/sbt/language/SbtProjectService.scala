package org.jetbrains.sbt
package language

import com.intellij.CommonBundle
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer
import com.intellij.notification.impl.NotificationsConfigurationImpl
import com.intellij.notification.{Notification, NotificationDisplayType, NotificationGroupManager}
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.{ApplicationManager, TransactionGuard}
import com.intellij.openapi.externalSystem.service.notification.{ExternalSystemNotificationManager, NotificationCategory, NotificationData, NotificationSource}
import com.intellij.openapi.module.ModuleType
import com.intellij.openapi.options.ex.SingleConfigurableEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ui.configuration.ProjectStructureConfigurable
import com.intellij.openapi.ui.Messages
import com.intellij.psi.{PsiManager, PsiTreeChangeAdapter, PsiTreeChangeEvent}
import com.intellij.util.Consumer
import org.jetbrains.annotations.Nls
import org.jetbrains.idea.maven.indices.{MavenIndex, MavenIndicesManager}
import org.jetbrains.sbt.project.SbtProjectSystem
import org.jetbrains.sbt.project.module.SbtModuleType
import org.jetbrains.sbt.resolvers.indexes.IvyIndex
import org.jetbrains.sbt.resolvers.{SbtMavenRepositoryProvider, SbtResolverUtils}

import java.util
import javax.swing.event.HyperlinkEvent
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

/**
 * @author Pavel Fatin
 */
final class SbtProjectService(project: Project) extends Disposable {

  private val SBT_MAVEN_NOTIFICATION_GROUP_ID = SbtBundle.message("sbt.unindexed.maven.repositories.for.sbt.detection")
  private val SBT_MAVEN_NOTIFICATION_GROUP =
    NotificationGroupManager.getInstance.getNotificationGroup(SBT_MAVEN_NOTIFICATION_GROUP_ID)

  setupMavenIndexes()

  private def manager = PsiManager.getInstance(project)

  (manager.addPsiTreeChangeListener(TreeListener): @nowarn("cat=deprecation"))

  private def analyzer = DaemonCodeAnalyzer.getInstance(project)

  override def dispose(): Unit = {
    manager.removePsiTreeChangeListener(TreeListener)
  }

  object TreeListener extends PsiTreeChangeAdapter {
    override def childrenChanged(event: PsiTreeChangeEvent): Unit = {
      event.getFile match {
        case file: SbtFileImpl => analyzer.restart(file)
        case _ =>
      }
    }
  }

  val unindexedNotifier: Consumer[util.List[MavenIndex]] = new Consumer[util.List[MavenIndex]] {
    override def consume(mavenIndexes: util.List[MavenIndex]): Unit = {

      if (project.isDisposed) return
      val sbtRepos = (new SbtMavenRepositoryProvider)
        .getRemoteRepositories(project)
        .asScala
        .map(_.getUrl)
        .toSet

      val unindexedRepos = mavenIndexes
        .asScala
        .filter(idx => idx.getUpdateTimestamp == -1 && sbtRepos.contains(idx.getRepositoryPathOrUrl))

      if (unindexedRepos.isEmpty) return
      val title = SbtBundle.message("sbt.unindexed.maven.repositories.found", unindexedRepos.length.toString)
      val message =
        SbtBundle.message("sbt.unindexed.maven.repositories.found.message").stripMargin
      val notificationData = createIndexerNotification(title, message)
      ExternalSystemNotificationManager.getInstance(project).showNotification(SbtProjectSystem.Id, notificationData)
    }
  }

  private def setupMavenIndexes(): Unit = {
    if (ApplicationManager.getApplication.isUnitTestMode) return

    if (isIdeaPluginEnabled("org.jetbrains.idea.maven")) {
      MavenIndicesManager.getInstance(project).scheduleUpdateIndicesList(null)
    } else {
      notifyDisabledMavenPlugin()
    }
  }

  private def notifyDisabledMavenPlugin(): Unit = {
    val outdatedIvyIndexes = SbtResolverUtils.projectResolvers(project)
      .flatMap(_.getIndex(project))
      .filter {
        case index: IvyIndex => index.getUpdateTimeStamp == -1
        case _ => false
      }

    if (outdatedIvyIndexes.isEmpty) return
    val title = SbtBundle.message("unindexed.ivy.repositories.found", outdatedIvyIndexes.size)
    val message = SbtBundle.message("update.repositories.to.use.dependency.completion").stripMargin
    val notificationData = createIndexerNotification(title, message)
    ExternalSystemNotificationManager.getInstance(project).showNotification(SbtProjectSystem.Id, notificationData)
  }

  def createIndexerNotification(@Nls title: String, @Nls message: String): NotificationData = {
    val notificationData = new NotificationData(
      title,
      message,
      NotificationCategory.WARNING,
      NotificationSource.PROJECT_SYNC)
    notificationData.setBalloonNotification(true)
    notificationData.setBalloonGroup(SBT_MAVEN_NOTIFICATION_GROUP)
    notificationData.setListener(
      "#open", (notification: Notification, e: HyperlinkEvent) => {
        val ui = ProjectStructureConfigurable.getInstance(project)
        val editor = new SingleConfigurableEditor(project, ui)
        val module = ui.getModulesConfig.getModules.find(ModuleType.get(_).isInstanceOf[SbtModuleType])
        ui.select(module.get.getName, "sbt", false)
        //Project Structure should be shown in a transaction
        TransactionGuard.getInstance().submitTransactionAndWait(() => editor.show()): @nowarn("cat=deprecation")
      })
    notificationData.setListener(
      "#disable", (notification: Notification, e: HyperlinkEvent) => {
        val result: Int = Messages.showYesNoDialog(project,
          SbtBundle.message("sbt.notification.will.be.disabled.for.all.projects", SBT_MAVEN_NOTIFICATION_GROUP_ID),
          SbtBundle.message("sbt.unindexed.maven.repositories.sbt.detection"),
          SbtBundle.message("sbt.disable.notification"),
          CommonBundle.getCancelButtonText,
          Messages.getWarningIcon)
        if (result == Messages.YES) {
          NotificationsConfigurationImpl.getInstanceImpl.changeSettings(SBT_MAVEN_NOTIFICATION_GROUP_ID, NotificationDisplayType.NONE, false, false)
          notification.hideBalloon()
        }
      })
    notificationData
  }
}

object SbtProjectService {
  def getInstance(project: Project): SbtProjectService =
    project.getService(classOf[SbtProjectService])
}