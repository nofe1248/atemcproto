module;

#include <cstdint>
#include <format>
#include <string>
#include <utility>

export module Atem.AST.Expr.Type;

import Atem.AST.Utils;

export namespace atem::ast {
    struct BoolType {
        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("BoolType at {:#018x}", reinterpret_cast<uintptr_t>(this));
        }
    };
    struct UnitType {
        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("UnitType at {:#018x}", reinterpret_cast<uintptr_t>(this));
        }
    };
    struct NoreturnType {
        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("NoreturnType at {:#018x}", reinterpret_cast<uintptr_t>(this));
        }
    };
    struct SignedIndexType {
        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("SignedIndexType at {:#018x}", reinterpret_cast<uintptr_t>(this));
        }
    };
    struct UnsignedIndexType {
        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("UnsignedIndexType at {:#018x}", reinterpret_cast<uintptr_t>(this));
        }
    };
    struct IntType {
        std::uint64_t width;
        bool sign;

        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("IntType at {:#018x}, width = {}, sign = {}", reinterpret_cast<uintptr_t>(this), this->width, this->sign);
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

        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("FloatType at {:#018x}, width = {}", reinterpret_cast<uintptr_t>(this), std::to_underlying(this->width));
        }
    };
    struct StringType {
        std::size_t length;

        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("StringType at {:#018x}, length = {}", reinterpret_cast<uintptr_t>(this), this->length);
        }
    };
    struct RuneType {
        [[nodiscard]] constexpr auto toString() const -> std::string {
            return std::format("RuneType at {:#018x}", reinterpret_cast<uintptr_t>(this));
        }
    };
    struct BuiltinTypeExpr : utils::VariantASTBase<BuiltinTypeExpr, BoolType, UnitType, NoreturnType, SignedIndexType,
                                               UnsignedIndexType, IntType, FloatType, StringType, RuneType> {};
    struct TypeExpr : utils::VariantASTBase<TypeExpr, BuiltinTypeExpr> {};
} // namespace atem::ast
