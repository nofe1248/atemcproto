module;

#include <memory>
#include <print>

#include <libassert/assert.hpp>

#include <lexy/action/trace.hpp>
#include <lexy/input/file.hpp>

export module Atem.Main;

import Atem.AST;
import Atem.Parser.Grammar;

export auto main(int const argc, char **argv) -> int {
    using namespace atem;

    auto ast_tree = ast::SourceFileAST::make({ast::Decl::make(ast::FunctionDecl::make(
            "main", {}, ast::TypeExpr::make(ast::BuiltinTypeExpr::make(ast::IntType::make(32, true))),
            std::make_shared<ast::Expr>(ast::BlockExpr::make(
                    {std::make_shared<ast::Expr>(ast::Decl::make(ast::VariableDecl::make(
                             "ret", ast::TypeExpr::make(ast::BuiltinTypeExpr::make(ast::IntType::make(32, true))),
                             std::make_shared<ast::Expr>(
                                     ast::LiteralExpr::make(ast::IntegerLiteral::make("42", 10)))))),
                     std::make_shared<ast::Expr>(ast::ReturnExpr::make(
                             std::make_shared<ast::Expr>(ast::IdentifierExpr::make("ret"))))}))))});

    std::println("{}", ast::ASTPrettyPrinter::toString(ast_tree));

    if (argc < 2) {
        std::println("usage: %s <filename>", argv[0]);
        return 1;
    }

    auto file = lexy::read_file<lexy::utf8_encoding>(argv[1]);
    if (not file) {
        std::println("file {} not found", argv[1]);
        return 1;
    }

    lexy::trace<grammar::SourceFile>(stdout, file.buffer(), {.flags = lexy::visualization_flags::visualize_use_color});
    return 0;
}
