module;

#include <lexy/callback.hpp>
#include <lexy/dsl.hpp>
#include <lexy_ext/report_error.hpp>

export module Atem.Parser.Grammar;

// Custom Parsing Errors
export namespace atem::grammar::error {
    struct UnexpectedTrailingComma {
        static constexpr auto name = "unexpected trailing comma in the list";
    };

    struct InvalidCharInString {
        static constexpr auto name = "invalid character in string literal";
    };
} // namespace atem::grammar::error

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

    struct BoolLiteralExpression : lexy::token_production {
        static constexpr auto rule = keyword_true | keyword_false;
    };

    struct IntLiteralExpression : lexy::token_production {
        struct DecimalInteger : lexy::transparent_production {
            static constexpr auto rule = dsl::sign + dsl::digits<dsl::decimal>.sep(dsl::digit_sep_underscore);
        };
        static constexpr auto rule = [] {
            constexpr auto binary_integer = LEXY_LIT("0b") >> dsl::digits<dsl::binary>.sep(dsl::digit_sep_underscore);
            constexpr auto octal_integer = LEXY_LIT("0o") >> dsl::digits<dsl::octal>.sep(dsl::digit_sep_underscore);
            constexpr auto decimal_integer =
                    dsl::peek(dsl::lit_c<'-'> / dsl::lit_c<'+'> / dsl::digit<>) >> dsl::p<DecimalInteger>;
            constexpr auto hexadecimal_integer = LEXY_LIT("0x") >> dsl::digits<dsl::hex>.sep(dsl::digit_sep_underscore);

            return binary_integer | octal_integer | hexadecimal_integer | decimal_integer;
        }();
    };

    struct FloatLiteralExpression : lexy::token_production {
        static constexpr auto rule = [] {
            constexpr auto integral_part = dsl::sign + dsl::digits<>;
            constexpr auto fraction_part = dsl::period >> dsl::digits<>;
            constexpr auto exponent_part = dsl::lit_c<'e'> / dsl::lit_c<'E'> >> dsl::sign + dsl::digits<>;

            constexpr auto float_literal = dsl::token(integral_part + fraction_part + dsl::if_(exponent_part));
            return dsl::capture(float_literal);
        }();
    };

    struct StringLiteralExpression : lexy::token_production {
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
            constexpr auto code_point = (-dsl::unicode::control).error<error::InvalidCharInString>;
            constexpr auto escape = dsl::backslash_escape.symbol<escape_sequences>().rule(dsl::p<CodePointId>);

            return dsl::quoted.limit(dsl::ascii::newline)(code_point, escape);
        }();
    };

    struct NullLiteralExpression : lexy::token_production {
        static constexpr auto rule = keyword_null;
    };

    struct UndefinedLiteralExpression : lexy::token_production {
        static constexpr auto rule = keyword_undefined;
    };

    struct UnitLiteralExpression : lexy::token_production {
        static constexpr auto rule = LEXY_LIT("()");
    };

    struct LiteralExpression {
        static constexpr auto rule = dsl::p<BoolLiteralExpression> | dsl::p<FloatLiteralExpression> |
                                     dsl::p<IntLiteralExpression> | dsl::p<StringLiteralExpression> |
                                     dsl::p<NullLiteralExpression> | dsl::p<UndefinedLiteralExpression> |
                                     dsl::p<UnitLiteralExpression>;
    };

    struct BoolTypeExpression {
        static constexpr auto rule = keyword_bool;
    };

    struct IntTypeExpression {
        static constexpr auto rule =
                keyword_int >> dsl::integer<std::uint64_t>(dsl::digits<>.no_leading_zero()) | keyword_isize;
    };

    struct UIntTypeExpression {
        static constexpr auto rule =
                keyword_uint >> dsl::integer<std::uint64_t>(dsl::digits<>.no_leading_zero()) | keyword_usize;
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
        static constexpr auto rule = dsl::p<Identifier>;
    };

    struct TypeExpression {
        static constexpr auto rule = dsl::p<BuiltinTypeExpression> | dsl::p<IdentifierTypeExpression>;
    };

    struct IdentifierExpression {
        static constexpr auto rule = dsl::p<Identifier>;
    };

    struct Expression;
    struct ArithmeticExpression;

    struct BlockExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = dsl::curly_bracketed.opt_list(dsl::recurse<Expression>);
    };

    struct ParenthesizedExpression {
        static constexpr auto rule = dsl::parenthesized(dsl::recurse<Expression>);
    };

    struct FunctionCallExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto function_name = dsl::p<Identifier>;
            constexpr auto function_parameter_list =
                    dsl::parenthesized.opt_list(dsl::recurse<ArithmeticExpression>,
                                                dsl::sep(dsl::comma).trailing_error<error::UnexpectedTrailingComma>);

            return function_name + function_parameter_list;
        }();
    };

    struct PrimaryExpression {
        static constexpr auto rule = dsl::peek(dsl::p<Identifier> + dsl::lit_c<'('>) >> dsl::p<FunctionCallExpression> |
                                     dsl::p<LiteralExpression> | dsl::p<IdentifierExpression> | dsl::p<TypeExpression> |
                                     dsl::p<ParenthesizedExpression>;
    };

    struct ArithmeticExpression : lexy::expression_production {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto atom = dsl::p<PrimaryExpression>;

        struct PrefixOp : dsl::prefix_op {
            static constexpr auto op = dsl::op(dsl::lit_c<'-'>) / dsl::op(keyword_not) / dsl::op(dsl::lit_c<'~'>);
            using operand = dsl::atom;
        };

        struct MultiplicativeOp : dsl::infix_op_left {
            static constexpr auto op = dsl::op(dsl::lit_c<'*'>) / dsl::op(dsl::lit_c<'/'>) / dsl::op(dsl::lit_c<'%'>) /
                                       dsl::op(LEXY_LIT("**"));
            using operand = PrefixOp;
        };

        struct AdditiveOp : dsl::infix_op_left {
            static constexpr auto op = dsl::op(dsl::lit_c<'+'>) / dsl::op(dsl::lit_c<'-'>);
            using operand = MultiplicativeOp;
        };

        struct ShiftingOp : dsl::infix_op_left {
            static constexpr auto op = dsl::op(LEXY_LIT("<<")) / dsl::op(LEXY_LIT(">>"));
            using operand = AdditiveOp;
        };

        struct BinaryBitwiseOp : dsl::infix_op_left {
            static constexpr auto op = dsl::op(dsl::lit_c<'&'>) / dsl::op(dsl::lit_c<'|'>) / dsl::op(dsl::lit_c<'^'>);
            using operand = ShiftingOp;
        };

        struct RelationalOp : dsl::infix_op_left {
            static constexpr auto op = dsl::op(LEXY_LIT("==")) / dsl::op(LEXY_LIT("!=")) / dsl::op(dsl::lit_c<'<'>) /
                                       dsl::op(dsl::lit_c<'>'>) / dsl::op(LEXY_LIT("<=")) / dsl::op(LEXY_LIT(">="));
            using operand = BinaryBitwiseOp;
        };

        struct LogicalAndOp : dsl::infix_op_left {
            static constexpr auto op = dsl::op(keyword_and);
            using operand = RelationalOp;
        };

        struct LogicalOrOp : dsl::infix_op_left {
            static constexpr auto op = dsl::op(keyword_or);
            using operand = LogicalAndOp;
        };

        struct AssignmentOp : dsl::infix_op_single {
            static constexpr auto op = dsl::op(dsl::lit_c<'='>) / dsl::op(LEXY_LIT("+=")) / dsl::op(LEXY_LIT("-=")) /
                                       dsl::op(LEXY_LIT("*=")) / dsl::op(LEXY_LIT("/=")) / dsl::op(LEXY_LIT("%=")) /
                                       dsl::op(LEXY_LIT("**=")) / dsl::op(LEXY_LIT("&=")) / dsl::op(LEXY_LIT("|=")) /
                                       dsl::op(LEXY_LIT("^=")) / dsl::op(LEXY_LIT("<<=")) / dsl::op(LEXY_LIT(">>="));
            using operand = LogicalOrOp;
        };

        using operation = AssignmentOp;
    };

    struct IfExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto if_cond = dsl::p<ArithmeticExpression>;
            constexpr auto true_expr = dsl::peek(dsl::lit_c<'{'>) >> dsl::p<BlockExpression> |
                                       keyword_then >> dsl::p<ArithmeticExpression>;
            constexpr auto false_expr = dsl::peek(dsl::lit_c<'{'>) >> dsl::p<BlockExpression> |
                                        dsl::peek(keyword_if) >> dsl::recurse<IfExpression> |
                                        dsl::else_ >> dsl::p<ArithmeticExpression>;

            return keyword_if >> if_cond + true_expr + dsl::opt(keyword_else >> false_expr);
        }();
    };

    struct ReturnExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = keyword_return >> dsl::recurse<Expression>;
    };

    struct WhileExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto while_cond = dsl::p<ArithmeticExpression>;
            constexpr auto loop_expr = dsl::peek(dsl::lit_c<'{'>) >> dsl::p<BlockExpression> |
                                       keyword_then >> dsl::p<ArithmeticExpression>;
            constexpr auto else_expr =
                    dsl::peek(dsl::lit_c<'{'>) >> dsl::p<BlockExpression> | dsl::else_ >> dsl::p<ArithmeticExpression>;

            return keyword_while >> while_cond + loop_expr + dsl::opt(keyword_else >> else_expr);
        }();
    };

    struct BreakExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto break_expr = keyword_break + dsl::p<ArithmeticExpression>;

            return keyword_break | dsl::peek(keyword_break) >> break_expr;
        }();
    };

    struct ContinueExpression {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = keyword_continue;
    };

    struct FunctionDeclarationHeader {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = dsl::p<Identifier> + LEXY_LIT(":") + keyword_function;
    };

    struct FunctionDeclaration {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto argument_name = identifier;
            [[maybe_unused]] constexpr auto argument_type = dsl::p<TypeExpression>;
            [[maybe_unused]] constexpr auto function_argument = argument_name + LEXY_LIT(":") + argument_type;

            constexpr auto argument_list = dsl::parenthesized.opt_list(function_argument, dsl::sep(dsl::comma));

            constexpr auto return_type = dsl::p<TypeExpression>;
            constexpr auto body = dsl::p<BlockExpression>;

            return dsl::p<FunctionDeclarationHeader> + argument_list + dsl::opt(LEXY_LIT("->") >> return_type) +
                   LEXY_LIT("=") + body;
        }();
    };

    struct VariableDeclarationHeader {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = dsl::p<Identifier> + LEXY_LIT(":") + keyword_var;
    };

    struct VariableDeclaration {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto variable_initializer = dsl::recurse<Expression>;
            constexpr auto variable_type = dsl::p<TypeExpression>;

            return dsl::p<VariableDeclarationHeader> + variable_type + dsl::opt(dsl::lit_c<'='> >> variable_initializer);
        }();
    };

    struct ConstantDeclarationHeader {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = dsl::p<Identifier> + LEXY_LIT(":") + keyword_const;
    };

    struct ConstantDeclaration {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto constant_initializer = dsl::recurse<Expression>;
            constexpr auto constant_type = dsl::p<TypeExpression>;

            return dsl::p<ConstantDeclarationHeader> + constant_type + dsl::lit_c<'='> + constant_initializer;
        }();
    };

    struct Expression {
        static constexpr auto rule =
                dsl::peek(dsl::lit_c<'{'>) >> dsl::p<BlockExpression> |
                dsl::peek(keyword_if) >> dsl::p<IfExpression> | dsl::peek(keyword_while) >> dsl::p<WhileExpression> |
                dsl::peek(keyword_return) >> dsl::p<ReturnExpression> |
                dsl::peek(dsl::p<FunctionDeclarationHeader>) >> dsl::p<FunctionDeclaration> |
                dsl::peek(dsl::p<VariableDeclarationHeader>) >> dsl::p<VariableDeclaration> |
                dsl::peek(dsl::p<ConstantDeclarationHeader>) >> dsl::p<ConstantDeclaration> |
                dsl::else_ >> dsl::p<ArithmeticExpression>;
    };

    struct Declaration {
        static constexpr auto rule = dsl::peek(dsl::p<FunctionDeclarationHeader>) >> dsl::p<FunctionDeclaration> |
                                     dsl::peek(dsl::p<VariableDeclarationHeader>) >> dsl::p<VariableDeclaration> |
                                     dsl::peek(dsl::p<ConstantDeclarationHeader>) >> dsl::p<ConstantDeclaration>;
    };

    struct SourceFile {
        static constexpr auto whitespace = whitespace_or_comment;
        static constexpr auto rule = [] {
            constexpr auto decl = dsl::p<Declaration>;

            constexpr auto recoverable_decl =
                    dsl::peek(dsl::p<FunctionDeclarationHeader>) >> dsl::p<FunctionDeclaration> |
                    dsl::peek(dsl::p<VariableDeclarationHeader>) >> dsl::p<VariableDeclaration> |
                    dsl::peek(dsl::p<ConstantDeclarationHeader>) >> dsl::p<ConstantDeclaration>;
            constexpr auto decl_recover = dsl::recover(recoverable_decl);
            constexpr auto try_decl = dsl::try_(decl, decl_recover);

            return dsl::terminator(dsl::eof).list(try_decl);
        }();
    };
} // namespace atem::grammar
