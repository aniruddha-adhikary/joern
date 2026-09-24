package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.parser.ARLParser

import scala.jdk.CollectionConverters.*

/** Resolution of ARL type references to typeFullNames.
  *
  *   - primitives pass through unchanged
  *   - an explicit `import a.b.C` resolves the simple name `C` to `a.b.C`
  *   - the `java.lang` set below resolves without an import
  *   - already-qualified names pass through unchanged
  *   - generics are erased to the raw name (the original code text is kept on nodes)
  *   - array suffixes `[]` are preserved
  *   - anything else stays a simple name; the XOM linker rewrites those later
  */
trait TypeResolver {
  this: AstCreator =>

  /** Explicit `import a.b.C` declarations of the current file. */
  protected var explicitImports: List[String] = List.empty

  private val javaLangAuto: Set[String] = Set(
    "String",
    "Object",
    "Integer",
    "Long",
    "Double",
    "Float",
    "Boolean",
    "Character",
    "Short",
    "Byte",
    "Number",
    "Math",
    "System"
  )

  private val primitives: Set[String] =
    Set("boolean", "byte", "short", "int", "long", "char", "float", "double", "void")

  /** Resolve a (possibly qualified, possibly generic, possibly array) type name. */
  protected def resolveTypeName(rawName: String): String = {
    val arraySuffix  = "[]" * ("\\[\\s*\\]".r.findAllMatchIn(rawName).size)
    val withoutArray = rawName.replace("[]", "").replace("[ ]", "").trim
    val erased       = withoutArray.takeWhile(_ != '<').trim
    val resolved     = resolveSimpleOrQualified(erased)
    resolved + arraySuffix
  }

  /** Resolve a bare qualified-name string (no generics, no array suffix). */
  protected def resolveSimpleOrQualified(name: String): String = {
    if (primitives.contains(name)) {
      name
    } else if (name.contains('.')) {
      name
    } else {
      explicitImports.collectFirst { case imp if imp.endsWith(s".$name") => imp } match {
        case Some(qualified) => qualified
        case None            => if (javaLangAuto.contains(name)) s"java.lang.$name" else name
      }
    }
  }

  /** TypeFullName for a `type` context: qualifiedName with optional generics and array dims, or a primitive. */
  protected def typeFullName(typeCtx: ARLParser.TypeContext): String = {
    if (typeCtx == null) return "ANY"
    val dims = "[]" * (if (typeCtx.getText != null) "\\[\\s*\\]".r.findAllMatchIn(typeCtx.getText).size else 0)
    val base =
      if (typeCtx.primitiveType() != null) {
        typeCtx.primitiveType().getText
      } else {
        val qn = typeCtx.qualifiedName()
        resolveSimpleOrQualified(qualifiedNameText(qn))
      }
    base + dims
  }

  /** The raw (unresolved) text of a qualifiedName, with backticks stripped from each segment. */
  protected def qualifiedNameText(qn: ARLParser.QualifiedNameContext): String =
    qualifiedNameSegments(qn).mkString(".")

  /** The individual segments of a qualifiedName (`id` contexts, terminals elsewhere). */
  protected def qualifiedNameSegments(qn: ARLParser.QualifiedNameContext): List[String] =
    childrenOf(qn)
      .filter(child => child.isInstanceOf[ARLParser.IdContext] || child.getText != ".")
      .map(child => stripBackticks(child.getText))
      .filter(_.nonEmpty)

  protected def stripBackticks(text: String): String = {
    if (text.startsWith("`") && text.endsWith("`") && text.length >= 2) text.substring(1, text.length - 1) else text
  }

  /** True when a simple name plausibly denotes a type usable in a static context: resolvable via imports/java.lang, or
    * Uppercase-initial (Java class-name convention).
    */
  protected def isLikelyTypeName(name: String): Boolean = {
    name.contains('.') ||
    primitives.contains(name) ||
    javaLangAuto.contains(name) ||
    explicitImports.exists(imp => imp.endsWith(s".$name") || imp == name) ||
    name.headOption.exists(_.isUpper)
  }
}
