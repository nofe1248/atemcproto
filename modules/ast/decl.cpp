module;

#include <vector>

export module Atem.AST.Decl;

import Atem.AST.Utils;

export namespace atem::ast {
    struct FunctionDecl {

    };

    struct Decl : utils::VariantASTBase<Decl, FunctionDecl> {};

    struct SourceFileAST {
        std::vector<Decl> value;
    };
}