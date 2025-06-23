module;

#include <lexy/callback.hpp>
#include <lexy/dsl.hpp>

export module Atem.Parser.Grammar;

// Grammar for Atem language
export namespace atemc::grammar {
    namespace dsl = lexy::dsl;

    constexpr auto oneline_comment = LEXY_LIT("//") >> dsl::until(dsl::ascii::newline);

    constexpr auto identifier = dsl::identifier(dsl::unicode::xid_start_underscore, dsl::unicode::xid_continue);
    constexpr auto keyword_and = LEXY_KEYWORD("and", identifier);
    constexpr auto keyword_as = LEXY_KEYWORD("as", identifier);
    constexpr auto keyword_bool = LEXY_KEYWORD("Bool", identifier);
    constexpr auto keyword_break = LEXY_KEYWORD("break", identifier);
    constexpr auto keyword_const = LEXY_KEYWORD("const", identifier);
    constexpr auto keyword_continue = LEXY_KEYWORD("continue", identifier);
    constexpr auto keyword_do = LEXY_KEYWORD("do", identifier);
    constexpr auto keyword_else = LEXY_KEYWORD("else", identifier);
    constexpr auto keyword_false = LEXY_KEYWORD("false", identifier);
    constexpr auto keyword_float16 = LEXY_KEYWORD("Float16", identifier);
    constexpr auto keyword_float32 = LEXY_KEYWORD("Float32", identifier);
    constexpr auto keyword_float64 = LEXY_KEYWORD("Float64", identifier);
    constexpr auto keyword_float80 = LEXY_KEYWORD("Float80", identifier);
    constexpr auto keyword_float128 = LEXY_KEYWORD("Float128", identifier);
    constexpr auto keyword_for = LEXY_KEYWORD("for", identifier);
    constexpr auto keyword_function = LEXY_KEYWORD("function", identifier);
    constexpr auto keyword_if = LEXY_KEYWORD("if", identifier);
    constexpr auto keyword_int = LEXY_KEYWORD("Int", identifier);
    constexpr auto keyword_into = LEXY_KEYWORD("into", identifier);
    constexpr auto keyword_is = LEXY_KEYWORD("is", identifier);
    constexpr auto keyword_isize = LEXY_KEYWORD("ISize", identifier);
    constexpr auto keyword_match = LEXY_KEYWORD("match", identifier);
    constexpr auto keyword_noreturn = LEXY_KEYWORD("Noreturn", identifier);
    constexpr auto keyword_not = LEXY_KEYWORD("not", identifier);
    constexpr auto keyword_null = LEXY_KEYWORD("null", identifier);
    constexpr auto keyword_or = LEXY_KEYWORD("or", identifier);
    constexpr auto keyword_return = LEXY_KEYWORD("return", identifier);
    constexpr auto keyword_rune = LEXY_KEYWORD("Rune", identifier);
    constexpr auto keyword_step = LEXY_KEYWORD("step", identifier);
    constexpr auto keyword_string = LEXY_KEYWORD("String", identifier);
    constexpr auto keyword_struct = LEXY_KEYWORD("struct", identifier);
    constexpr auto keyword_then = LEXY_KEYWORD("then", identifier);
    constexpr auto keyword_true = LEXY_KEYWORD("true", identifier);
    constexpr auto keyword_type = LEXY_KEYWORD("type", identifier);
    constexpr auto keyword_uint = LEXY_KEYWORD("UInt", identifier);
    constexpr auto keyword_undefined = LEXY_KEYWORD("undefined", identifier);
    constexpr auto keyword_usize = LEXY_KEYWORD("USize", identifier);
    constexpr auto keyword_unit = LEXY_KEYWORD("Unit", identifier);
    constexpr auto keyword_var = LEXY_KEYWORD("var", identifier);
    constexpr auto keyword_while = LEXY_KEYWORD("while", identifier);
    constexpr auto keyword_with = LEXY_KEYWORD("with", identifier);

    struct Identifier {
        static constexpr auto rule = [] {
            // An identifier is
            return identifier
                    // not a keyword
                    .reserve(keyword_and, keyword_as, keyword_bool, keyword_break, keyword_const, keyword_continue,
                             keyword_do, keyword_else, keyword_false, keyword_float16, keyword_float32, keyword_float64,
                             keyword_float80, keyword_float128, keyword_for, keyword_function, keyword_if, keyword_int,
                             keyword_into, keyword_is, keyword_isize, keyword_match, keyword_noreturn, keyword_not,
                             keyword_null, keyword_or, keyword_return, keyword_rune, keyword_step, keyword_string,
                             keyword_struct, keyword_then, keyword_true, keyword_type, keyword_uint, keyword_undefined,
                             keyword_usize, keyword_unit, keyword_var, keyword_while, keyword_with)
                    // doesn't start with an underscore
                    .reserve_prefix(dsl::lit_c<'_'>)
                    // or contains a double underscore
                    .reserve_containing(LEXY_LIT("__"));
        }();
    };
} // namespace atemc::grammar
