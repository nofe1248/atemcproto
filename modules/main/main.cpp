module;

#include <print>

#include <libassert/assert.hpp>

#include <lexy/action/trace.hpp>
#include <lexy/input/file.hpp>

export module Atem.Main;

import Atem.AST;
import Atem.Parser.Grammar;

export auto main(int const argc, char **argv) -> int {
    if (argc < 2) {
        std::println("usage: %s <filename>", argv[0]);
        return 1;
    }

    auto file = lexy::read_file<lexy::utf8_encoding>(argv[1]);
    if (not file) {
        std::println("file {} not found", argv[1]);
        return 1;
    }

    lexy::trace<atem::grammar::SourceFile>(stdout, file.buffer(),
                                           {.flags = lexy::visualization_flags::visualize_use_color});
    return 0;
}
