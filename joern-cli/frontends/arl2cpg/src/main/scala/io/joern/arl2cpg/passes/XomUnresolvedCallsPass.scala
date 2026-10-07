package io.joern.arl2cpg.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.{Codes, Keys}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

class XomUnresolvedCallsPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.call.l.filter(isUnresolvedXomCall).foreach { call =>
      val filename       = call.file.name.headOption.getOrElse("")
      val methodFullName = call.start
        .repeat(_.astParent)(_.until(_.isMethod))
        .isMethod
        .fullName
        .headOption
        .getOrElse(throw new IllegalStateException(s"XOM call '${call.code}' has no enclosing method"))
      val receiverType = call.receiver
        .nextOption()
        .flatMap(_.propertyOption(PropertyNames.TypeFullName))
        .getOrElse("")
      ArlFindings.finding(
        builder,
        Some(call),
        Codes.UnresolvedCallTarget,
        "xom-body",
        s"unresolved call '${call.code}' in XOM method $methodFullName",
        filename,
        call.lineNumber,
        List(Keys.CallId -> call.id().toString, Keys.ReceiverType -> receiverType, "method" -> methodFullName)
      )
    }
  }

  private def isUnresolvedXomCall(call: Call): Boolean =
    call.file.name.headOption.exists(filename => !filename.endsWith(".arl")) &&
      !call.name.startsWith("<operator>") &&
      (call.methodFullName.contains(Defines.UnresolvedSignature) ||
        call.methodFullName.contains(Defines.UnresolvedNamespace))
}
