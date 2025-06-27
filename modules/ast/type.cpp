module;

#include <cstdint>
#include <format>
#include <string>

#include <llvm/ADT/StringRef.h>

export module Atem.AST.Expr.Type;

import Atem.AST.Utils;

export namespace atem::ast {
    struct BoolType {
        [[nodiscard]] static constexpr auto make() -> BoolType {
            return BoolType{};
        }
    };
    struct UnitType {
        [[nodiscard]] static constexpr auto make() -> UnitType {
            return UnitType{};
        }
    };
    struct NoreturnType {
        [[nodiscard]] static constexpr auto make() -> NoreturnType {
            return NoreturnType{};
        }
    };
    struct SignedIndexType {
        [[nodiscard]] static constexpr auto make() -> SignedIndexType {
            return SignedIndexType{};
        }
    };
    struct UnsignedIndexType {
        [[nodiscard]] static constexpr auto make() -> UnsignedIndexType {
            return UnsignedIndexType{};
        }
    };
    struct IntType {
        std::uint64_t width;
        bool sign;

        [[nodiscard]] static constexpr auto make(std::uint64_t const width, bool const sign) -> IntType {
            return IntType{.width = width, .sign = sign};
        }
    };
    enum class FloatTypeWidth : std::uint8_t {
        FLOAT_WIDTH_16 = 16,
        FLOAT_WIDTH_32 = 32,
        FLOAT_WIDTH_64 = 64,
        FLOAT_WIDTH_80 = 80,
        FLOAT_WIDTH_128 = 128,
    };
    struct FloatType {
        FloatTypeWidth width;

        [[nodiscard]] static constexpr auto make(FloatTypeWidth const width) -> FloatType {
            return FloatType{.width = width};
        }
    };
    struct StringType {
        [[nodiscard]] static constexpr auto make() -> StringType {
            return StringType{};
        }
    };
    struct RuneType {
        [[nodiscard]] static constexpr auto make() -> RuneType {
            return RuneType{};
        }
    };
    struct BuiltinTypeExpr : utils::VariantASTBase<BuiltinTypeExpr, BoolType, UnitType, NoreturnType, SignedIndexType,
                                                   UnsignedIndexType, IntType, FloatType, StringType, RuneType> {};
    struct IdentifierType {
        std::string identifier;

        [[nodiscard]] static constexpr auto make(llvm::StringRef const identifier) -> IdentifierType {
            return IdentifierType{.identifier = identifier.str()};
        }
    };
    struct TypeExpr : utils::VariantASTBase<TypeExpr, BuiltinTypeExpr, IdentifierType> {};
} // namespace atem::ast
