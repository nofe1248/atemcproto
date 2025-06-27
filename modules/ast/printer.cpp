module;

#include <format>
#include <string>

#include <llvm/ADT/SmallString.h>

export module Atem.AST.Printer;

import Atem.AST.Expr;

export namespace atem::ast {
    struct ASTPrettyPrinter {
        [[nodiscard]] static constexpr auto toString(UnaryExpr const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(BinaryExpr const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(IdentifierExpr const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(BlockExpr const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(FunctionCallExpr const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(IfExpr const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(WhileExpr const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(ReturnExpr const &ast, std::size_t ident = 0) -> std::string;

        [[nodiscard]] static constexpr auto toString(FunctionDecl const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(VariableDecl const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(ConstantDecl const &ast, std::size_t ident = 0) -> std::string;

        [[nodiscard]] static constexpr auto toString(IntegerLiteral const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(FloatLiteral const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(BoolLiteral const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(NullLiteral const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(UndefinedLiteral const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(StringLiteral const &ast, std::size_t ident = 0) -> std::string;

        [[nodiscard]] static constexpr auto toString(BoolType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(UnitType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(UnsignedIndexType const &ast, std::size_t ident = 0)
                -> std::string;
        [[nodiscard]] static constexpr auto toString(SignedIndexType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(IntType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(FloatType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(NoreturnType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(RuneType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(StringType const &ast, std::size_t ident = 0) -> std::string;
        [[nodiscard]] static constexpr auto toString(IdentifierType const &ast, std::size_t ident = 0) -> std::string;

        [[nodiscard]] static constexpr auto toString(SourceFileAST const &ast, std::size_t ident = 0) -> std::string;

        template<typename T>
            requires requires { typename T::variant_ast_tag; }
        [[nodiscard]] static constexpr auto toString(T const &ast, std::size_t ident = 0) -> std::string {
            return ast.value.visit([ident](auto const &child) { return toString(child, ident); });
        }

    private:
        [[nodiscard]] static constexpr auto getIdentStr(std::size_t const ident) -> std::string {
            std::string ident_str = "";
            if (ident > 0) {
                ident_str.append("|-").append(std::string((ident - 1) * 2, '-'));
            }
            return ident_str;
        }
    };

    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(UnaryExpr const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#UnaryExpr at 0x{:016x}, op = {}, operand =\n{}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), operatorToString(ast.op),
                           toString(*ast.operand, ident + 1));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(BinaryExpr const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#BinaryExpr at 0x{:016x}, op = {}, lhs =\n{}\n{}rhs=\n{}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), operatorToString(ast.op).str(),
                           toString(*ast.lhs, ident + 1), ident_str, toString(*ast.rhs, ident + 1));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(IdentifierExpr const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#IdentifierExpr at 0x{:016x}, identifier = {}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.identifier);
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(BlockExpr const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        std::string result = std::format("{}#BlockExpr at 0x{:016x}, body =", ident_str,
                                         reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
        for (auto const &child: ast.expressions) {
            result.append(std::format("\n{}", toString(*child, ident + 1)));
        }
        return result;
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(FunctionCallExpr const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        std::string result = std::format("{}#FunctionCallExpr at 0x{:016x}, name = {}, arguments =", ident_str,
                                         reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.name);
        for (auto const &child: ast.arguments) {
            result.append(std::format("\n{}", toString(*child, ident + 1)));
        }
        return result;
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(IfExpr const &ast, std::size_t const ident) -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#IfExpr at 0x{:016x}, condition = \n{}\n{}true_branch =\n{}\n{}false_branch =\n{}",
                           ident_str, reinterpret_cast<std::uintptr_t>(std::addressof(ast)),
                           toString(*ast.condition, ident + 1), ident_str, toString(*ast.true_branch, ident + 1),
                           ident_str,
                           ast.false_branch.has_value() ? toString(*ast.false_branch.value(), ident + 1) : "<none>");
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(WhileExpr const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#WhileExpr at 0x{:016x}, condition = \n{}\n{}body =\n{}\n{}else_branch =\n{}",
                           ident_str, reinterpret_cast<std::uintptr_t>(std::addressof(ast)),
                           toString(*ast.condition, ident + 1), ident_str, toString(*ast.body, ident + 1), ident_str,
                           ast.else_branch.has_value() ? toString(*ast.else_branch.value(), ident + 1) : "<none>");
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(ReturnExpr const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#ReturnExpr at 0x{:016x}, expression = \n{}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), toString(*ast.expression, ident + 1));
    }

    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(FunctionDecl const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        std::string result = std::format("{}#FunctionDecl at 0x{:016x}, name = {}, parameter_types =", ident_str,
                                         reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.name);
        if (ast.parameter_types.empty()) {
            result.append(" <empty>");
        } else {
            for (auto const &child: ast.parameter_types) {
                result.append(std::format("\n{}", toString(child, ident + 1)));
            }
        }
        result.append(std::format("\n{}body = \n{}", ident_str, toString(*ast.body, ident + 1)));
        return result;
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(VariableDecl const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#VariableDecl at 0x{:016x}, name = {}, type = \n{}\n{}initializer = \n{}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.name,
                           toString(ast.type, ident + 1), ident_str,
                           ast.initializer.has_value() ? toString(*ast.initializer.value(), ident + 1) : "<none>");
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(ConstantDecl const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#ConstantDecl at 0x{:016x}, name = {}, type = \n{}\n{}initializer = \n{}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.name,
                           toString(ast.type, ident + 1), ident_str, toString(*ast.initializer, ident + 1));
    }

    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(IntegerLiteral const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        llvm::SmallString<64> apint;
        ast.value.toStringSigned(apint);
        return std::format("{}#IntegerLiteral at 0x{:016x}, value = {}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), apint.str());
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(FloatLiteral const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        llvm::SmallString<64> apint;
        ast.value.toString(apint);
        return std::format("{}#FloatLiteral at 0x{:016x}, value = {}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), apint.str());
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(BoolLiteral const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#BoolLiteral at 0x{:016x}, value = {}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.value);
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(NullLiteral const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#NullLiteral at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(UndefinedLiteral const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#UndefinedLiteral at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(StringLiteral const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#StringLiteral at 0x{:016x}, value = \"{}\"", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.value);
    }

    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(BoolType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#BoolType at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(UnitType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#UnitType at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(UnsignedIndexType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#UnsignedIndexType at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(SignedIndexType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#SignedIndexType at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(IntType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#IntType at 0x{:016x}, width = {}, sign = {}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.width, ast.sign);
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(FloatType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#FloatType at 0x{:016x}, width = {}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), std::to_underlying(ast.width));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(NoreturnType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#NoreturnType at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(RuneType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#RuneType at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(StringType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#StringType at 0x{:016x}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
    }
    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(IdentifierType const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        return std::format("{}#IdentifierType at 0x{:016x}, identifier = {}", ident_str,
                           reinterpret_cast<std::uintptr_t>(std::addressof(ast)), ast.identifier);
    }

    [[nodiscard]] constexpr auto ASTPrettyPrinter::toString(SourceFileAST const &ast, std::size_t const ident)
            -> std::string {
        std::string const ident_str = getIdentStr(ident);
        std::string result = std::format("{}#SourceFileAST at 0x{:016x}, value =", ident_str,
                                         reinterpret_cast<std::uintptr_t>(std::addressof(ast)));
        for (auto const &child: ast.value) {
            result.append(std::format("\n{}", toString(child, ident + 1)));
        }
        return result;
    }
} // namespace atem::ast
