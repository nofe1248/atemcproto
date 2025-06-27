module;

#include <cmath>
#include <string>

#include <libassert/assert.hpp>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallString.h>

export module Atem.AST.Expr.LiteralExpr;

import Atem.AST.Utils;

export namespace atem::ast {
    struct IntegerLiteral {
        llvm::APInt value;

        [[nodiscard]] static constexpr auto make(llvm::StringRef const value, std::uint8_t const radix) -> IntegerLiteral {
            llvm::APInt const result(
                    static_cast<unsigned>(std::pow(2, std::ceil(std::log2(llvm::APInt::getBitsNeeded(value, radix))))),
                    value, radix);
            return IntegerLiteral{.value = result};
        }
    };

    struct FloatLiteral {
        llvm::APFloat value;

        [[nodiscard]] static constexpr auto make(llvm::StringRef const value, llvm::fltSemantics const &semantics) -> FloatLiteral {
            llvm::APFloat const result(semantics, value);
            return FloatLiteral{.value = result};
        }
    };

    struct BoolLiteral {
        bool value;

        [[nodiscard]] static constexpr auto make(llvm::StringRef const value) -> BoolLiteral {
            if (value == "true") {
                return BoolLiteral{.value = true};
            }
            if (value == "false") {
                return BoolLiteral{.value = false};
            }
            UNREACHABLE("Invalid boolean literal", value.str());
        }
    };

    struct NullLiteral {
        [[nodiscard]] static constexpr auto make() -> NullLiteral {
            return NullLiteral{};
        }
    };

    struct UndefinedLiteral {
        [[nodiscard]] static constexpr auto make() -> UndefinedLiteral {
            return UndefinedLiteral{};
        }
    };

    struct StringLiteral {
        std::string value;

        [[nodiscard]] static constexpr auto make(llvm::StringRef const value) -> StringLiteral {
            return StringLiteral{.value = value.str()};
        }
    };

    struct LiteralExpr : utils::VariantASTBase<LiteralExpr, IntegerLiteral, FloatLiteral, BoolLiteral, NullLiteral,
                                               UndefinedLiteral, StringLiteral> {};
} // namespace atem::ast
