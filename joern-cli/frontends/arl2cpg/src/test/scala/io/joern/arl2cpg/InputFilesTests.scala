package io.joern.arl2cpg

import io.joern.arl2cpg.util.InputFiles
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{FileSystemLoopException, Files}

class InputFilesTests extends AnyWordSpec with Matchers {

  "InputFiles.walk" should {
    "return matching paths in sorted order" in {
      val root = Files.createTempDirectory("arl2cpg-input-files")
      Files.writeString(root.resolve("b.rfl"), "")
      Files.writeString(root.resolve("a.rfl"), "")

      InputFiles
        .walk(root, "--rfl-src", path => Files.isRegularFile(path) && path.toString.endsWith(".rfl"))
        .map(_.getFileName.toString) shouldBe List("a.rfl", "b.rfl")
    }

    "wrap filesystem loops while preserving the loop exception as a cause" in {
      val root = Files.createTempDirectory("arl2cpg-input-files-loop")
      val nested = Files.createDirectories(root.resolve("nested"))
      Files.createSymbolicLink(nested.resolve("back"), root)

      val exception = intercept[IllegalArgumentException] {
        InputFiles.walk(root, "--rfl-src", path => Files.isRegularFile(path))
      }
      exception.getMessage should include("--rfl-src")
      Iterator.iterate(exception: Throwable)(_.getCause)
        .takeWhile(_ != null)
        .exists(_.isInstanceOf[FileSystemLoopException]) shouldBe true
    }
  }
}
