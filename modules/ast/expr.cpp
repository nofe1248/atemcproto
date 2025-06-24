module;

#include <flat_map>
#include <memory>

#include <libassert/assert.hpp>

#include <llvm/ADT/StringRef.h>

export module Atem.AST.Expr;

export import Atem.AST.Expr.LiteralExpr;

import Atem.AST.Utils;

export namespace atem::ast {
    struct Expr;

    enum class Operator {
        OP_MINIMUM,
        OP_ADD,
        OP_SUB,
        OP_MUL,
        OP_DIV,
        OP_MOD,
        OP_POW,
        OP_LT,
        OP_GT,
        OP_LE,
        OP_GE,
        OP_EQ,
        OP_NE,
        OP_AND,
        OP_OR,
        OP_NOT,
        OP_MAXIMUM,
    };

    constexpr auto operatorToString(Operator const op) -> llvm::StringRef {
        using enum Operator;
        static const std::flat_map<Operator, llvm::StringRef> operators = {
            {OP_ADD, "+"},  {OP_SUB, "-"}, {OP_MUL, "*"},   {OP_DIV, "/"}, {OP_MOD, "%"},
            {OP_POW, "**"}, {OP_LT, "<"},  {OP_GT, ">"},    {OP_LE, "<="}, {OP_GE, ">="},
            {OP_EQ, "=="},  {OP_NE, "!="}, {OP_AND, "and"}, {OP_OR, "or"}, {OP_NOT, "not"}};
        ASSERT(op > OP_MINIMUM and op < OP_MAXIMUM, "Invalid enum value for atem::ast::Operator", op);
        return operators.at(op);
    }

    struct UnaryExpr {
        Operator op;
        std::shared_ptr<Expr> operand;

        static constexpr auto make(Operator const op, std::shared_ptr<Expr> operand) -> UnaryExpr {
            ASSERT(op > Operator::OP_MINIMUM and op < Operator::OP_MAXIMUM,
                   "Invalid enum value for atem::ast::Operator", op);
            return UnaryExpr{.op = op, .operand = operand};
        }
    };

    struct BinaryExpr {
        Operator op;
        std::shared_ptr<Expr> lhs;
        std::shared_ptr<Expr> rhs;

        static constexpr auto make(Operator const op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs) -> BinaryExpr {
            ASSERT(op > Operator::OP_MINIMUM and op < Operator::OP_MAXIMUM,
                   "Invalid enum value for atem::ast::Operator", op);
            return BinaryExpr{.op = op, .lhs = lhs, .rhs = rhs};
        }
    };

    struct Expr : utils::VariantASTBase<Expr, LiteralExpr, UnaryExpr, BinaryExpr> {};
}