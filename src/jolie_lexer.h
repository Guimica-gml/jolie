#ifndef JOLIE_LEXER_H
#define JOLIE_LEXER_H

#include "./utils.h"

#define JOLIE_LOC_FMT "%s:%zu:%zu"
#define JOLIE_LOC_ARG(loc) (loc).filepath, (loc).row, (loc).col

#define JOLIE_COMMENT_CHAR '#'

typedef enum {
    // Special
    JOLIE_END,
    JOLIE_WORD,

    // Symbols
    JOLIE_PAREN_OPEN,
    JOLIE_PAREN_CLOSE,
    JOLIE_CURLY_OPEN,
    JOLIE_CURLY_CLOSE,
    JOLIE_BRACK_OPEN,
    JOLIE_BRACK_CLOSE,
    JOLIE_SEMICOLON,
    JOLIE_COLON,
    JOLIE_CARET,
    JOLIE_EQUALS,
    JOLIE_PLUS,
    JOLIE_DASH,
    JOLIE_ASTERISK,
    JOLIE_SLASH,
    JOLIE_COMMA,
    JOLIE_LESS_THAN,

    // Keywords
    JOLIE_LET,
    JOLIE_PROC,
    JOLIE_IF,
    JOLIE_ELSE,
    JOLIE_WHILE,
    JOLIE_RETURN,
    JOLIE_CAST,

    // Literals
    JOLIE_UINT64_LIT,
    JOLIE_STRING_LIT,

    // Errors
    JOLIE_UNCLOSED_STRING,
    JOLIE_UNKNOWN_CHARACTER,

    JOLIE_TOKEN_COUNT,
} Jolie_Token_Type;

typedef struct {
    const char *filepath;
    size_t row;
    size_t col;
} Jolie_Loc;

typedef struct {
    String_View text;
    Jolie_Token_Type type;
    Jolie_Loc loc;
} Jolie_Token;

typedef struct {
    const char *src_filepath;
    String_View content;
    size_t index;
    size_t line;
    size_t bol;
} Jolie_Lexer;

const char *jolie_token_type_to_cstr(Jolie_Token_Type type);

Jolie_Lexer jolie_lexer_from_sv(const char *src_filepath, String_View content);
void jolie_consume_char(Jolie_Lexer *lexer);
String_View jolie_chop(Jolie_Lexer *lexer, size_t count);
String_View jolie_chop_until(Jolie_Lexer *lexer, char ch);
String_View jolie_chop_while(Jolie_Lexer *lexer, int(*predicate)(int));
bool jolie_starts_with(Jolie_Lexer *lexer, String_View sv);

int jolie_is_word_head(int ch);
int jolie_is_word_body(int ch);

Jolie_Token jolie_next_token(Jolie_Lexer *lexer);
void jolie_reset_lexer(Jolie_Lexer *lexer);

#endif // JOLIE_LEXER_H
