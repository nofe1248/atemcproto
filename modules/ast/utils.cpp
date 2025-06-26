module;

#include <algorithm>
#include <filesystem>
#include <variant>
#include <vector>

#include <libassert/assert.hpp>

#include <lexy/input/file.hpp>
#include <lexy/input_location.hpp>

export module Atem.AST.Utils;

import mp;

export namespace atem::ast::utils {
    struct ASTLocation {
        using location_t = lexy::input_location<lexy::buffer<lexy::utf8_encoding>>;
        location_t start_location;
        location_t end_location;
        std::filesystem::path file_path;

        static constexpr auto make(location_t const &start_location, location_t const &end_location,
                                   std::filesystem::path const &file_path) -> ASTLocation {
            ASSERT(std::filesystem::exists(file_path), "Specified source file does not exist", file_path.string());
            ASSERT(std::filesystem::is_regular_file(file_path), "Specified path is not a file", file_path.string());
            ASSERT(file_path.extension() == ".atem", "Specified source file does not have the correct extension",
                   file_path.string());

            return ASTLocation{.start_location = start_location, .end_location = end_location, .file_path = file_path};
        }
    };

    template<typename... Types>
    struct OverloadSet : Types... {
        using Types::operator()...;
    };

    template<typename TypeToFind, typename... Types>
    struct contain_type {
        static constexpr bool value = [] {
            auto vec = std::vector<mp::info>{mp::meta<Types>...};
            return std::ranges::contains(vec, mp::meta<TypeToFind>);
        }();
    };

    template<typename TypeToFind, typename... Types>
    constexpr bool contain_type_v = contain_type<TypeToFind, Types...>::value;

    template<typename Derived, typename... VariantTypes>
    struct VariantASTBase {
        std::variant<VariantTypes...> value;

        template<typename... Types>
        constexpr auto visit(Types &&...args) {
            return this->value.visit(OverloadSet(std::forward<Types>(args)...));
        }

        template<typename... Types>
        constexpr auto inexhaustiveVisit(Types &&...args) {
            return this->value.visit(OverloadSet(std::forward<Types>(args)..., [](auto &&) {}));
        }

        [[nodiscard]] constexpr auto toString() const -> std::string {
            return this->value.visit([]<typename T>(T &&value) { return std::forward<T>(value).toString(); });
        }

        template<typename T>
            requires contain_type_v<T, VariantTypes...>
        static constexpr auto make(T &&t) -> Derived {
            return Derived{std::forward<T>(t)};
        }
    };

    template<typename T>
    constexpr auto astToString(T &&node) -> std::string {
        return std::forward<T>(node).toString();
    }
} // namespace atem::ast::utils
