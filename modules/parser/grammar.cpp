module;

#include <lexy/callback.hpp>
#include <lexy/dsl.hpp>
#include <lexy_ext/report_error.hpp>

export module Atem.Parser.Grammar;

// Grammar for Atem language
export namespace atem::grammar {
    namespace dsl = lexy::dsl;

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

    constexpr auto whitespace_or_comment = dsl::ascii::space | dsl::ascii::newline |
                                           LEXY_LIT("//") >> dsl::until(dsl::newline).or_eof() |
                                           LEXY_LIT("/*") >> dsl::until(LEXY_LIT("*/"));

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

    struct BoolLiteralExpression {
        static constexpr auto rule = keyword_true | keyword_false;
    };

    struct IntLiteralExpression {
        struct BinaryIntLiteralExpression {
            static constexpr auto rule = LEXY_LIT("0b") >> dsl::digits<dsl::binary>.sep(dsl::digit_sep_underscore);
        };

        struct OctalIntLiteralExpression {
            static constexpr auto rule = LEXY_LIT("0o") >> dsl::digits<dsl::octal>.sep(dsl::digit_sep_underscore);
        };

        struct DecimalIntLiteralExpression {
            static constexpr auto rule = dsl::digits<dsl::decimal>.sep(dsl::digit_sep_underscore);
        };

        struct HexadecimalIntLiteralExpression {
            static constexpr auto rule = LEXY_LIT("0x") >> dsl::digits<dsl::hex>.sep(dsl::digit_sep_underscore);
        };

        static constexpr auto rule = dsl::peek(dsl::lit_c<'-'> / dsl::digit<>) >>
                                     (dsl::p<BinaryIntLiteralExpression> | dsl::p<OctalIntLiteralExpression> |
                                      dsl::p<HexadecimalIntLiteralExpression> | dsl::p<DecimalIntLiteralExpression>);
    };

    struct FloatLiteralExpression {
        struct IntegralPart {
            static constexpr auto rule = dsl::digits<dsl::decimal>.sep(dsl::digit_sep_underscore);
        };

        struct FractionPart {
            static constexpr auto rule = dsl::lit_c<'.'> >> dsl::digits<dsl::decimal>.sep(dsl::digit_sep_underscore);
        };

        struct ExponentialPart {
            static constexpr auto rule = [] {
                constexpr auto exp_char = dsl::lit_c<'e'> | dsl::lit_c<'E'>;
                return exp_char >> dsl::sign + dsl::digits<dsl::decimal>.sep(dsl::digit_sep_underscore);
            }();
        };

        static constexpr auto rule = dsl::peek(dsl::lit_c<'-'> / dsl::digit<>) >>
                                     dsl::p<IntegralPart> + dsl::p<FractionPart> + dsl::opt(dsl::p<ExponentialPart>);
    };

    struct StringLiteralExpression {
        struct InvalidCharError {
            static consteval auto name() {
                return "invalid character in string literal";
            }
        };

        static constexpr auto escape_sequences = lexy::symbol_table<char>
            .map<'"'>('"')
            .map<'\\'>('\\')
            .map<'/'>('/')
            .map<'b'>('\b')
            .map<'f'>('\f')
            .map<'n'>('\n')
            .map<'r'>('\r')
            .map<'t'>('\t');

        struct CodePointId {
            static constexpr auto rule = LEXY_LIT("u") >> dsl::code_unit_id<lexy::utf16_encoding, 4>;
        };

        static constexpr auto rule = [] {
            constexpr auto code_point = (-dsl::unicode::control).error<InvalidCharError>;
            constexpr auto escape = dsl::backslash_escape.symbol<escape_sequences>().rule(dsl::p<CodePointId>);

            return dsl::quoted.limit(dsl::ascii::newline)(code_point, escape);
        }();
    };

    struct NullLiteralExpression {
        static constexpr auto rule = keyword_null;
    };

    struct UndefinedLiteralExpression {
        static constexpr auto rule = keyword_undefined;
    };

    struct UnitLiteralExpression {
        static constexpr auto rule = LEXY_LIT("()");
    };

    struct LiteralExpression {
        static constexpr auto rule = dsl::p<BoolLiteralExpression> | dsl::p<IntLiteralExpression> |
                                     dsl::p<FloatLiteralExpression> | dsl::p<StringLiteralExpression> |
                                     dsl::p<NullLiteralExpression> | dsl::p<UndefinedLiteralExpression> |
                                     dsl::p<UnitLiteralExpression>;
    };

    struct BoolTypeExpression {
        static constexpr auto rule = keyword_bool;
    };

    struct IntTypeExpression {
        static constexpr auto rule =
                LEXY_LIT("Int") >> dsl::integer<std::uint64_t>(dsl::digits<>.no_leading_zero()) | keyword_isize;
    };

    struct UIntTypeExpression {
        static constexpr auto rule =
                LEXY_LIT("UInt") >> dsl::integer<std::uint64_t>(dsl::digits<>.no_leading_zero()) | keyword_usize;
    };

    struct FloatTypeExpression {
        static constexpr auto rule =
                keyword_float16 | keyword_float32 | keyword_float64 | keyword_float80 | keyword_float128;
    };

    struct NoreturnTypeExpression {
        static constexpr auto rule = keyword_noreturn;
    };

    struct UnitTypeExpression {
        static constexpr auto rule = keyword_unit;
    };

    struct StringTypeExpression {
        static constexpr auto rule = keyword_string;
    };

    struct RuneTypeExpression {
        static constexpr auto rule = keyword_rune;
    };

    struct BuiltinTypeExpression {
        static constexpr auto rule = dsl::p<BoolTypeExpression> | dsl::p<IntTypeExpression> |
                                     dsl::p<UIntTypeExpression> | dsl::p<FloatTypeExpression> |
                                     dsl::p<NoreturnTypeExpression> | dsl::p<UnitTypeExpression> |
                                     dsl::p<StringTypeExpression> | dsl::p<RuneTypeExpression>;
    };

    struct IdentifierTypeExpression {
        static constexpr auto rule = identifier;
    };

    struct TypeExpression {
        static constexpr auto rule = dsl::p<BuiltinTypeExpression> | dsl::p<IdentifierTypeExpression>;
    };

    struct Expression;

    struct BlockExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = dsl::curly_bracketed.opt_list(dsl::recurse<Expression>);
    };

    struct Expression {
        static constexpr auto rule = dsl::p<TypeExpression> | dsl::p<LiteralExpression>;
    };

    struct FunctionArgumentList {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto argument_name = identifier;
            [[maybe_unused]] constexpr auto argument_type = dsl::p<TypeExpression>;
            [[maybe_unused]] constexpr auto function_argument = argument_name + LEXY_LIT(":") + argument_type;

            return dsl::parenthesized.opt_list(function_argument, dsl::sep(dsl::comma));
        }();
    };

    struct FunctionDeclaration {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto function_name = identifier;
            constexpr auto arguments = dsl::p<FunctionArgumentList>;
            constexpr auto return_type = dsl::p<TypeExpression>;
            constexpr auto body = dsl::p<BlockExpression>;

            return dsl::position + function_name + LEXY_LIT(":") + keyword_function + arguments +
                   dsl::opt(LEXY_LIT("->") >> return_type) + LEXY_LIT("=") + body;
        }();
    };

    struct Declaration {
        static constexpr auto rule = dsl::p<FunctionDeclaration>;
    };

    struct SourceFile {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = dsl::terminator(dsl::eof).list(dsl::p<FunctionDeclaration>);
    };
} // namespace atem::grammar
