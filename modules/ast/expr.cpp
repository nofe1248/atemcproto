module;

#include <memory>
#include <string>
#include <utility>

#include <libassert/assert.hpp>

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>

export module Atem.AST.Expr;

export import Atem.AST.Expr.LiteralExpr;
export import Atem.AST.Expr.Type;

import Atem.AST.Utils;

export namespace atem::ast {
    struct Expr;

    enum class Operator {
        OP_MINIMUM,
        OP_NEG,
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
        OP_BW_NOT,
        OP_BW_AND,
        OP_BW_OR,
        OP_BW_XOR,
        OP_BW_SHL,
        OP_BW_SHR,
        OP_ASSIGN,
        OP_ADD_ASSIGN,
        OP_SUB_ASSIGN,
        OP_MUL_ASSIGN,
        OP_DIV_ASSIGN,
        OP_MOD_ASSIGN,
        OP_POW_ASSIGN,
        OP_BW_AND_ASSIGN,
        OP_BW_OR_ASSIGN,
        OP_BW_XOR_ASSIGN,
        OP_BW_SHL_ASSIGN,
        OP_BW_SHR_ASSIGN,
        OP_MAXIMUM,
    };

    constexpr auto operatorToString(Operator const op) -> llvm::StringRef {
        using enum Operator;
        static const auto operators_map = llvm::DenseMap<Operator, llvm::StringRef>{{OP_NEG, "-"},
                                                                                    {OP_ADD, "+"},
                                                                                    {OP_SUB, "-"},
                                                                                    {OP_MUL, "*"},
                                                                                    {OP_DIV, "/"},
                                                                                    {OP_MOD, "%"},
                                                                                    {OP_POW, "**"},
                                                                                    {OP_LT, "<"},
                                                                                    {OP_GT, ">"},
                                                                                    {OP_LE, "<="},
                                                                                    {OP_GE, ">="},
                                                                                    {OP_EQ, "=="},
                                                                                    {OP_NE, "!="},
                                                                                    {OP_AND, "and"},
                                                                                    {OP_OR, "or"},
                                                                                    {OP_NOT, "not"},
                                                                                    {OP_BW_NOT, "~"},
                                                                                    {OP_BW_AND, "&"},
                                                                                    {OP_BW_OR, "|"},
                                                                                    {OP_BW_XOR, "^"},
                                                                                    {OP_BW_SHL, "<<"},
                                                                                    {OP_BW_SHR, ">>"},
                                                                                    {OP_ASSIGN, "="},
                                                                                    {OP_ADD_ASSIGN, "+="},
                                                                                    {OP_SUB_ASSIGN, "-="},
                                                                                    {OP_MUL_ASSIGN, "*="},
                                                                                    {OP_DIV_ASSIGN, "/="},
                                                                                    {OP_MOD_ASSIGN, "%="},
                                                                                    {OP_POW_ASSIGN, "**="},
                                                                                    {OP_BW_AND_ASSIGN, "&="},
                                                                                    {OP_BW_OR_ASSIGN, "|="},
                                                                                    {OP_BW_XOR_ASSIGN, "^="},
                                                                                    {OP_BW_SHL_ASSIGN, "<<="},
                                                                                    {OP_BW_SHR_ASSIGN, ">>="}};
        ASSERT(op > OP_MINIMUM and op < OP_MAXIMUM, "Invalid enum value for atem::ast::Operator", op);
        return operators_map.lookup(op);
    }

    struct UnaryExpr {
        Operator op;
        std::shared_ptr<Expr> operand;

        static constexpr auto make(Operator const op, std::shared_ptr<Expr> operand) -> UnaryExpr {
            ASSERT(op > Operator::OP_MINIMUM and op < Operator::OP_MAXIMUM,
                   "Invalid enum value for atem::ast::Operator", op);
            DEBUG_ASSERT(operand.get() != nullptr, "Invalid operand expression pointer for atem::ast::UnaryExpr");
            return UnaryExpr{.op = op, .operand = std::move(operand)};
        }
    };

    struct BinaryExpr {
        Operator op;
        std::shared_ptr<Expr> lhs;
        std::shared_ptr<Expr> rhs;

        static constexpr auto make(Operator const op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs)
                -> BinaryExpr {
            ASSERT(op > Operator::OP_MINIMUM and op < Operator::OP_MAXIMUM,
                   "Invalid enum value for atem::ast::Operator", op);
            DEBUG_ASSERT(lhs.get() != nullptr, "Invalid lhs expression pointer for atem::ast::BinaryExpr");
            DEBUG_ASSERT(rhs.get() != nullptr, "Invalid rhs expression pointer for atem::ast::BinaryExpr");
            return BinaryExpr{.op = op, .lhs = std::move(lhs), .rhs = std::move(rhs)};
        }
    };

    struct BlockExpr {
        llvm::SmallVector<std::shared_ptr<Expr>> expressions;

        static constexpr auto make(llvm::SmallVector<std::shared_ptr<Expr>> expressions) -> BlockExpr {
            return BlockExpr{.expressions = std::move(expressions)};
        }
    };

    struct FunctionCallExpr {
        std::string name;
        llvm::SmallVector<std::shared_ptr<Expr>> arguments;

        static constexpr auto make(llvm::StringRef const name, llvm::SmallVector<std::shared_ptr<Expr>> arguments)
                -> FunctionCallExpr {
            ASSERT(not name.empty(), "Function name cannot be empty");
            return FunctionCallExpr{.name = name.str(), .arguments = std::move(arguments)};
        }
    };

    struct IfExpr {
        std::shared_ptr<Expr> condition;
        std::shared_ptr<Expr> true_branch;
        std::optional<std::shared_ptr<Expr>> false_branch;

        static constexpr auto make(std::shared_ptr<Expr> condition, std::shared_ptr<Expr> true_branch,
                                   std::optional<std::shared_ptr<Expr>> false_branch = std::nullopt) -> IfExpr {
            DEBUG_ASSERT(condition.get() != nullptr, "Invalid condition pointer for atem::ast::IfExpr");
            DEBUG_ASSERT(true_branch.get() != nullptr, "Invalid true_branch pointer for atem::ast::IfExpr");
            if (false_branch.has_value()) {
                DEBUG_ASSERT(false_branch.value().get() != nullptr,
                             "Invalid false_branch pointer for atem::ast::IfExpr");
            }
            return IfExpr{.condition = std::move(condition),
                          .true_branch = std::move(true_branch),
                          .false_branch = std::move(false_branch)};
        }
    };

    struct WhileExpr {
        std::shared_ptr<Expr> condition;
        std::shared_ptr<Expr> body;
        std::optional<std::shared_ptr<Expr>> else_branch;

        static constexpr auto make(std::shared_ptr<Expr> condition, std::shared_ptr<Expr> body,
                                   std::optional<std::shared_ptr<Expr>> else_branch = std::nullopt) -> WhileExpr {
            DEBUG_ASSERT(condition.get() != nullptr, "Invalid condition pointer for atem::ast::WhileExpr");
            DEBUG_ASSERT(body.get() != nullptr, "Invalid body pointer for atem::ast::WhileExpr");
            if (else_branch.has_value()) {
                DEBUG_ASSERT(else_branch.value().get() != nullptr,
                             "Invalid else_branch pointer for atem::ast::WhileExpr");
            }
            return WhileExpr{
                    .condition = std::move(condition), .body = std::move(body), .else_branch = std::move(else_branch)};
        }
    };

    struct ReturnExpr {
        std::shared_ptr<Expr> expression;

        static constexpr auto make(std::shared_ptr<Expr> expression) -> ReturnExpr {
            DEBUG_ASSERT(expression.get() != nullptr, "Invalid expression pointer for atem::ast::ReturnExpr");
            return ReturnExpr{.expression = std::move(expression)};
        }
    };

    struct FunctionDecl {
        std::string name;
        llvm::SmallVector<TypeExpr> parameter_types;
        TypeExpr return_type;

        static constexpr auto make(llvm::StringRef const name, llvm::SmallVector<TypeExpr> parameter_types,
                                   TypeExpr const &return_type) -> FunctionDecl {
            ASSERT(not name.empty(), "Function name cannot be empty");
            return FunctionDecl{
                    .name = name.str(), .parameter_types = std::move(parameter_types), .return_type = return_type};
        }
    };

    struct VariableDecl {
        std::string name;
        TypeExpr type;
        std::optional<std::shared_ptr<Expr>> initializer;

        static constexpr auto make(llvm::StringRef const name, TypeExpr const &type,
                                   std::optional<std::shared_ptr<Expr>> initializer = std::nullopt) -> VariableDecl {
            ASSERT(not name.empty(), "Variable name cannot be empty");
            if (initializer.has_value()) {
                DEBUG_ASSERT(initializer.value() != nullptr, "Invalid initializer for atem::ast::VariableDecl", name);
            }
            return VariableDecl{.name = name.str(), .type = type, .initializer = std::move(initializer)};
        }
    };

    struct ConstantDecl {
        std::string name;
        TypeExpr type;
        std::shared_ptr<Expr> initializer;

        static constexpr auto make(llvm::StringRef const name, TypeExpr const &type, std::shared_ptr<Expr> initializer)
                -> ConstantDecl {
            ASSERT(not name.empty(), "Constant name cannot be empty");
            if (initializer) {
                DEBUG_ASSERT(initializer.get() != nullptr, "Invalid initializer for atem::ast::ConstantDecl", name);
            }
            return ConstantDecl{.name = name.str(), .type = type, .initializer = std::move(initializer)};
        }

        [[nodiscard]] constexpr auto toString() const -> std::string {
            std::string result = std::format("ConstantDecl at {:#018x}, name = {}", reinterpret_cast<uintptr_t>(this), this->name);
            result.append("\tType = ").append(this->type.toString()).append("\n");
            result.append("\tInitializer = ").append(utils::astToString(*this->initializer)).append("\n");
            return result;
        }
    };

    struct Decl : utils::VariantASTBase<Decl, FunctionDecl, VariableDecl, ConstantDecl> {};

    struct SourceFileAST {
        llvm::SmallVector<Decl> value;

        [[nodiscard]] constexpr auto toString() const -> std::string {
            std::string result;
            for (auto const &decl : this->value) {
                result.append(decl.toString()).append("\n");
            }
            return result;
        }
    };

    struct Expr : utils::VariantASTBase<Expr, LiteralExpr, UnaryExpr, BinaryExpr, TypeExpr, Decl, BlockExpr,
                                        FunctionCallExpr, IfExpr, WhileExpr, ReturnExpr> {};
} // namespace atem::ast
