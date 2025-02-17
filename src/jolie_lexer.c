#include "./jolie_lexer.h"

#include <ctype.h>

typedef struct {
    String_View text;
    Jolie_Token_Type type;
} Jolie_Literal_Token;

static_assert(JOLIE_TOKEN_COUNT == 27, "Count of tokens changed");
Jolie_Literal_Token jolie_literal_tokens[] = {
    { .text = SV_STATIC("("), .type = JOLIE_PAREN_OPEN },
    { .text = SV_STATIC(")"), .type = JOLIE_PAREN_CLOSE },
    { .text = SV_STATIC("{"), .type = JOLIE_CURLY_OPEN },
    { .text = SV_STATIC("}"), .type = JOLIE_CURLY_CLOSE },
    { .text = SV_STATIC("["), .type = JOLIE_BRACK_OPEN },
    { .text = SV_STATIC("]"), .type = JOLIE_BRACK_CLOSE },
    { .text = SV_STATIC(";"), .type = JOLIE_SEMICOLON },
    { .text = SV_STATIC(":"), .type = JOLIE_COLON },
    { .text = SV_STATIC("^"), .type = JOLIE_CARET },
    { .text = SV_STATIC("="), .type = JOLIE_EQUALS },
    { .text = SV_STATIC("+"), .type = JOLIE_PLUS },
    { .text = SV_STATIC("-"), .type = JOLIE_DASH },
    { .text = SV_STATIC("*"), .type = JOLIE_ASTERISK },
    { .text = SV_STATIC("/"), .type = JOLIE_SLASH },
    { .text = SV_STATIC(","), .type = JOLIE_COMMA },
    { .text = SV_STATIC("<"), .type = JOLIE_LESS_THAN },

    { .text = SV_STATIC("let"), .type = JOLIE_LET },
    { .text = SV_STATIC("proc"), .type = JOLIE_PROC },
    { .text = SV_STATIC("if"), .type = JOLIE_IF },
    { .text = SV_STATIC("while"), .type = JOLIE_WHILE },
    { .text = SV_STATIC("return"), .type = JOLIE_RETURN },
};
size_t jolie_literal_tokens_count =
    sizeof(jolie_literal_tokens)/sizeof(*jolie_literal_tokens);

static_assert(JOLIE_TOKEN_COUNT == 27, "Count of tokens changed");
const char *jolie_token_type_to_cstr(Jolie_Token_Type type) {
    switch (type) {
    case JOLIE_END: return "JOLIE_END";
    case JOLIE_WORD: return "JOLIE_WORD";
    case JOLIE_PAREN_OPEN: return "JOLIE_PAREN_OPEN";
    case JOLIE_PAREN_CLOSE: return "JOLIE_PAREN_CLOSE";
    case JOLIE_CURLY_OPEN: return "JOLIE_CURLY_OPEN";
    case JOLIE_CURLY_CLOSE: return "JOLIE_CURLY_CLOSE";
    case JOLIE_BRACK_OPEN: return "JOLIE_BRACK_OPEN";
    case JOLIE_BRACK_CLOSE: return "JOLIE_BRACK_CLOSE";
    case JOLIE_SEMICOLON: return "JOLIE_SEMICOLON";
    case JOLIE_COLON: return "JOLIE_COLON";
    case JOLIE_CARET: return "JOLIE_CARET";
    case JOLIE_EQUALS: return "JOLIE_EQUALS";
    case JOLIE_PLUS: return "JOLIE_PLUS";
    case JOLIE_DASH: return "JOLIE_DASH";
    case JOLIE_ASTERISK: return "JOLIE_ASTERISK";
    case JOLIE_SLASH: return "JOLIE_SLASH";
    case JOLIE_COMMA: return "JOLIE_COMMA";
    case JOLIE_LESS_THAN: return "JOLIE_LESS_THAN";

    case JOLIE_LET: return "JOLIE_LET";
    case JOLIE_PROC: return "JOLIE_PROC";
    case JOLIE_IF: return "JOLIE_IF";
    case JOLIE_WHILE: return "JOLIE_WHILE";
    case JOLIE_RETURN: return "JOLIE_RETURN";

    case JOLIE_UINT64_LIT: return "JOLIE_UINT64_LIT";
    case JOLIE_STRING_LIT: return "JOLIE_STRING_LIT";
    case JOLIE_UNCLOSED_STRING: return "JOLIE_UNCLOSED_STRING";
    case JOLIE_UNKNOWN_CHARACTER: return "JOLIE_UNKNOWN_CHARACTER";
    default: assert(0 && "unreachable");
    }
}

Jolie_Lexer jolie_lexer_from_sv(const char *src_filepath, String_View content) {
    Jolie_Lexer lexer = {0};
    lexer.src_filepath = src_filepath;
    lexer.content = content;
    return lexer;
}

void jolie_consume_char(Jolie_Lexer *lexer) {
    assert(lexer->index < lexer->content.size);
    if (lexer->content.data[lexer->index] == '\n') {
        lexer->line += 1;
        lexer->bol = lexer->index + 1;
    }
    lexer->index += 1;
}

String_View jolie_chop(Jolie_Lexer *lexer, size_t count) {
    String_View text = sv_from_parts(&lexer->content.data[lexer->index], count);
    for (size_t i = 0; i < count; ++i) {
        jolie_consume_char(lexer);
    }
    return text;
}

String_View jolie_chop_until(Jolie_Lexer *lexer, char ch) {
    size_t count = 0;
    while (lexer->index + count < lexer->content.size
           && ch != lexer->content.data[lexer->index + count])
    {
        count += 1;
    }
    return jolie_chop(lexer, count);
}

String_View jolie_chop_while(Jolie_Lexer *lexer, int(*predicate)(int)) {
    size_t count = 0;
    while (lexer->index + count < lexer->content.size
           && predicate(lexer->content.data[lexer->index + count]))
    {
        count += 1;
    }
    return jolie_chop(lexer, count);
}

bool jolie_starts_with(Jolie_Lexer *lexer, String_View sv) {
    if (lexer->index + sv.size >= lexer->content.size) {
        return false;
    }
    return memcmp(&lexer->content.data[lexer->index], sv.data, sv.size) == 0;
}

int jolie_is_word_head(int ch) {
    return isalpha(ch) || ch == '_';
}

int jolie_is_word_body(int ch) {
    return isalnum(ch) || ch == '_';
}

Jolie_Token jolie_next_token(Jolie_Lexer *lexer) {
again:
    jolie_chop_while(lexer, isspace);

    Jolie_Token token = {0};
    token.loc.filepath = lexer->src_filepath;
    token.loc.col = lexer->index - lexer->bol + 1;
    token.loc.row = lexer->line + 1;

    if (lexer->index >= lexer->content.size) {
        token.type = JOLIE_END;
        token.text = SV("<eof>");
        return token;
    }

    char peek = lexer->content.data[lexer->index];
    if (peek == JOLIE_COMMENT_CHAR) {
        jolie_chop_until(lexer, '\n');
        goto again;
    }

    for (size_t i = 0; i < jolie_literal_tokens_count; ++i) {
        if (jolie_starts_with(lexer, jolie_literal_tokens[i].text)) {
            token.type = jolie_literal_tokens[i].type;
            token.text = jolie_chop(lexer, jolie_literal_tokens[i].text.size);
            return token;
        }
    }

    if (peek == '"') {
        jolie_consume_char(lexer);
        const char *begin = lexer->content.data + lexer->index;
        size_t begin_index = lexer->index;

        bool escaped = false;
        while (true) {
            if (lexer->index >= lexer->content.size
                || lexer->content.data[lexer->index] == '\n')
            {
                token.type = JOLIE_UNCLOSED_STRING;
                token.text = sv_from_parts(begin, lexer->index - begin_index - 1);
                return token;
            }

            char ch = lexer->content.data[lexer->index];
            jolie_consume_char(lexer);

            if (!escaped && ch == '\"') {
                break;
            }
            escaped = (!escaped && ch == '\\');
        }

        token.type = JOLIE_STRING_LIT;
        token.text = sv_from_parts(begin, lexer->index - begin_index - 1);
        return token;
    }

    if (jolie_is_word_head(peek)) {
        token.type = JOLIE_WORD;
        token.text = jolie_chop_while(lexer, jolie_is_word_body);
        return token;
    }

    if (isdigit(peek)) {
        token.type = JOLIE_UINT64_LIT;
        token.text = jolie_chop_while(lexer, isalnum);
        return token;
    }

    token.type = JOLIE_UNKNOWN_CHARACTER;
    token.text = jolie_chop(lexer, 1);
    return token;
}

void jolie_reset_lexer(Jolie_Lexer *lexer) {
    lexer->index = 0;
    lexer->bol = 0;
    lexer->line = 0;
}
