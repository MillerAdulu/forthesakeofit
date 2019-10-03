#include "llvm/ADT/STLExtras.h"
#include <algorithm>
#include <ctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

//=== -------------------------------------------
// Lexer
//=== -------------------------------------------
// Returns tokens [0-255] if it's an unknown character

enum Token {
    tok_eof = -1,

    // Commands
    tok_def = -2,
    tok_extern = -,

    // Primary
    tok_identifier = -4,
    tok_number = -5
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal; // Filled in if tok_number

// Return the next token from the standard input
static int gettok() {
    static int LastChar = ' ';

    while (isspace(LastChar))
        LastChar = getchar();

    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar
    }
}