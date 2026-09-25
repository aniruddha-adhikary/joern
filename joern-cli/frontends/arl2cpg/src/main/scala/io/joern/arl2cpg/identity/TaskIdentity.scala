package io.joern.arl2cpg.identity

import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Exact declaration-site key: the ARL file basename, the 1-based declaration line and the visible task name. */
case class TaskIdentityKey(file: String, line: Int, task: String)

/** One sidecar record tying a flattened task declaration to its authored ruleflow uuid. */
case class TaskIdentityRecord(
  key: TaskIdentityKey,
  uuid: String,
  qualifiedName: Option[String],
  source: String,
  arlSha256: Option[String],
  path: String
)

/** The records of a task identity sidecar that apply to one ARL file.
  *
  * `verified` reports whether every applied record carried an `arlSha256` that matches the file's bytes; records
  * without a hash are still applied but leave `verified` false.
  */
final case class TaskIdentityFile(
  records: Map[TaskIdentityKey, TaskIdentityRecord],
  tasksByUuid: Map[String, Set[String]],
  verified: Boolean
)

object TaskIdentityFile {
  val empty: TaskIdentityFile = TaskIdentityFile(Map.empty, Map.empty, verified = false)
}

/** Compiler-produced task identity sidecars (JSON Lines, one object per line).
  *
  * Each record carries `file` (basenamed), `line`, `task` and `uuid`; `qualifiedName`, `source` and `arlSha256` are
  * optional. Records are joined on (file, line, task) — never on source adjacency — and win over `.rfl` inference.
  */
object TaskIdentity {
  private val logger = LoggerFactory.getLogger(getClass)

  def sha256Hex(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(byte => f"$byte%02x").mkString

  /** Load every sidecar under `paths` (files, or directories searched recursively for `*.jsonl`). Malformed lines and
    * records missing a required field are warned about and skipped; two records colliding on the same key with
    * different uuids are both dropped — a conflict means unknown, never a guess.
    */
  def load(paths: Seq[String]): TaskIdentityIndex = {
    val files = paths.toList.flatMap { path =>
      val root = Paths.get(path)
      if (Files.isRegularFile(root)) List(root)
      else if (Files.isDirectory(root)) {
        val stream = Files.walk(root)
        try {
          stream.iterator().asScala.toList.filter(file => Files.isRegularFile(file) && file.toString.endsWith(".jsonl"))
        } finally stream.close()
      } else {
        logger.warn(s"--task-identity '$path' is not a file or directory; skipping")
        List.empty
      }
    }
    val parsed = files.flatMap(parseFile)
    // Group by key; conflicting uuids on the same key drop every record for it.
    val conflicts = parsed.groupBy(_.key).filter { case (_, recs) => recs.map(_.uuid).distinct.size > 1 }
    conflicts.foreach { case (key, recs) =>
      logger.warn(
        s"task identity records for '${key.file}:${key.line}:${key.task}' disagree on the uuid " +
          s"(${recs.map(rec => s"${rec.uuid} @ ${rec.path}").mkString(", ")}); dropping all of them"
      )
    }
    new TaskIdentityIndex(parsed.groupBy(_.key).view.mapValues(_.head).toMap -- conflicts.keys)
  }

  private def parseFile(path: Path): List[TaskIdentityRecord] = {
    val lines = Try(Files.readAllLines(path).asScala.toList).getOrElse {
      logger.warn(s"Failed to read task identity sidecar '$path'; skipping")
      List.empty
    }
    lines.zipWithIndex.flatMap { case (line, idx) =>
      val trimmed = line.trim
      if (trimmed.isEmpty) None else parseLine(trimmed, path, idx + 1)
    }
  }

  private def parseLine(line: String, path: Path, lineNo: Int): Option[TaskIdentityRecord] = {
    def fail(reason: String): Option[TaskIdentityRecord] = {
      logger.warn(s"Malformed task identity record at '$path:$lineNo' ($reason); skipping")
      None
    }
    Try(ujson.read(line)) match {
      case scala.util.Failure(exception) => fail(exception.getMessage)
      case scala.util.Success(value)     =>
        Try(value.obj) match {
          case scala.util.Failure(_)   => fail("not a JSON object")
          case scala.util.Success(obj) =>
            def str(field: String): Option[String] =
              obj.get(field).flatMap(fieldValue => Try(fieldValue.str).toOption).filter(_.nonEmpty)
            val lineField = obj.get("line").flatMap(fieldValue => Try(fieldValue.num.toInt).toOption)
            (str("file"), lineField, str("task"), str("uuid")) match {
              case (Some(file), Some(declLine), Some(task), Some(uuid)) =>
                val basename = file.replace('\\', '/').split('/').lastOption.getOrElse(file)
                Option(
                  TaskIdentityRecord(
                    TaskIdentityKey(basename, declLine, task),
                    uuid,
                    str("qualifiedName"),
                    str("source").getOrElse("sidecar"),
                    str("arlSha256"),
                    path.toString
                  )
                )
              case _ => fail("missing required field(s): file, line, task, uuid")
            }
        }
    }
  }
}

final class TaskIdentityIndex(records: Map[TaskIdentityKey, TaskIdentityRecord]) {
  private val logger = LoggerFactory.getLogger(getClass)

  def isEmpty: Boolean = records.isEmpty

  /** The records applying to one ARL file. Any record carrying an `arlSha256` that does not match the file's actual
    * hash means the sidecar was produced for different input: all of its records for this file are ignored.
    */
  def forFile(fileBasename: String, actualSha256: String): TaskIdentityFile = {
    val forFile = records.filter { case (key, _) => key.file == fileBasename }
    if (forFile.isEmpty) return TaskIdentityFile.empty
    val mismatched = forFile.values.filter(rec => rec.arlSha256.exists(_ != actualSha256)).toList
    if (mismatched.nonEmpty) {
      logger.warn(
        s"task identity sidecar '${mismatched.head.path}' was produced for a different '$fileBasename' " +
          s"(sha mismatch); ignoring its ${forFile.size} records"
      )
      TaskIdentityFile.empty
    } else {
      val tasksByUuid = forFile.values.toList
        .groupBy(_.uuid)
        .view
        .mapValues(recs => recs.flatMap(rec => Set(rec.key.task, rec.key.task.split('>').last.trim)).toSet)
        .toMap
      val verified = forFile.values.forall(_.arlSha256.contains(actualSha256))
      TaskIdentityFile(forFile, tasksByUuid, verified)
    }
  }
}
