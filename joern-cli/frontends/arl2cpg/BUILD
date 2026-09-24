load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

java_binary(
    name = "antlr_tool",
    main_class = "org.antlr.v4.Tool",
    runtime_deps = [
        "@maven//:org_antlr_antlr4",
    ],
)

genrule(
    name = "generated_parser_code",
    srcs = ["src/main/antlr4/io/joern/arl2cpg/parser/ARL.g4"],
    outs = ["generated_classes.srcjar"],
    cmd = """
  $(location :antlr_tool) -package io.joern.arl2cpg.parser $< -o $(RULEDIR)/generated_classes
  $(JAVABASE)/bin/jar --create --file $@ -C $(RULEDIR)/generated_classes .
  """,
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    tools = [":antlr_tool"],
)

scala_library(
    name = "arl2cpg",
    srcs = glob(["src/main/scala/**/*.scala"]) + [":generated_parser_code"],
    scalacopts = [
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/javasrc2cpg",
        "//joern-cli/frontends/x2cpg",
        "//semanticcpg",
        "@maven//:org_antlr_antlr4_runtime",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    resources = glob(["src/test/resources/**"]),
    deps = [
        ":arl2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "//semanticcpg",
    ],
)

java_binary(
    name = "arl2cpg-bin",
    main_class = "io.joern.arl2cpg.Main",
    resources = ["src/main/resources/log4j2.xml"],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":arl2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "arl2cpg-dist",
    srcs = [":arl2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
