import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlExport

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

@main def main(cpgFile: String, outFile: String): Unit = {
  val cpg = importCpg(cpgFile).getOrElse {
    throw new IllegalArgumentException(s"Unable to import CPG from $cpgFile")
  }
  X2Cpg.applyDefaultOverlays(cpg)
  Files.writeString(Path.of(outFile), ArlExport.toJson(cpg), StandardCharsets.UTF_8)
}
