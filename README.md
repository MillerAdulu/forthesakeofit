# ForTheSakeOfIt

## Description

This is a personal implementation of a programming language called `ForTheSakeOfIt` to demonstrate concepts of making a programming language for the sake of it. Already there are multiple languages world over that achieve a lot of things and this is no way intended to even come close to what is already in existence.

Source: [LLVM Compiler Infrastructure](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)

## Procedure

### 1.  The Lexer

This is a sort of scanner that's intended to process a text file and recognize what it says. It breaks up the input text into `tokens` which include a `token code` and `some metadata`.

For our example, the lexer returns *tokens [0 - 255]* if the character is unknown, otherwise the following:

#### Tokens

```c++
enum Token {
    // End of File
    token_eof = -1,
    
    // Commands
    token_def = -2,
    token_extern = -3,
    
    // Primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal; // Filled in if tok_number
```

The `gettok` function is called to return the next token from standard input.

#### Return tokens

```c++
// gettok - Return the next token from the standard input
static int gettok() {
    static int LastChar = ' ';
    
    // Skip any whitespace
    while (isspace(LastChar))
        LastChar = getchar();
}
```

`gettok` calls the C `getchar()` function to read characters one at a time from standard input. It 'eats' them as it recognizes them and stores the last character read but not processed in LastChar. Whitespaces between tokens are ignored.

`gettok` goes ahead to recognize identifiers and specific key words like `def`. `ForTheSakeOfIt` accomplishes this with this a loop.

#### Lex Characters

```c++
if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
        IdentifierStr += LastChar;
    
    if(IdentifierStr == "def")
        return tok_def;
    if(IdentifierStr == "extern")
        return tok_extern;
    return tok_identifier;
}
```

`IdentifierStr` is set globally whenever it lexes an identifier.

#### Lex Digits

```c++
if(isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
        NumStr += LastChar;
        LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');
    
    NumVal = strtod(NumStr.c_str(), 0);
    return tok_number;
}
```

When reading a numeric value from input, the C `strtod` function is used to convert it to a numeric value that is stored in `NumVal`.

#### Lex Comments

Comments are lexed by skipping to the end of the line and then returning the next token

```c++
if (LastChar == "#") { // Comment until end of line
    do
        LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
    
    if (LastChar != EOF)
        return gettok();
}
```

#### Lex Special Characters

If the input doesn't match one of the above cases, it's either an operator character like `+` or the `end of the file EOF`

```c++
// Check for end of the file. Don't 'eat' the EOF.
if (LastChar == EOF)
    return tok_eof;

// Otherwise, just return the character as its ascii value
int ThisChar = LastChar;
LastChar = getchar();
return ThisChar;
```



### 2. Parser & AST

The lexer will now be used to build the parser for `ForTheSakeOfIt`. After the parser is built, the `Abstract Syntax Tree (AST)` will then be defined and built.

The parser uses a combination of `Recursive Descent Parsing` and `Operator-Precedence Parsing` go parse the `ForTheSakeOfIt` language.

1. Recursive Descent Parsing

A kind of top-down parser built from a set of mutually recursive procedures (or a non-recursive equivalent) where each such procedure implements one of the nonterminals of the grammar.

Thus the structure of the resulting program closely mirrors that of the grammar it recognizes.

`Top-down parser`: A parsing strategy where one first looks at the highest level of the parse tree and works down the parse tree by using the rewriting rules of a formal grammar.

`Parse/Parsing/Derivation/Concrete Syntax tree`: An ordered, rooted tree that represents the syntactic structure of a string according to some context-free grammar.

`Formal grammar`: A set of rules for rewriting strings, along with a "start symbol" from which rewriting starts.

`Mutually recursive`: A form of recursion where two mathematical or computational objects such as functions or data types, are defined in terms of each other.

2. Operator-Precedence Parsing

A bottom-up parser that interprets an operator-precedence grammar. It's a simple shift-reduce parser that's capable of parsing a subset of LR(1) grammars.

`Bottom-up parser` : Recognizes the text's lowest-level small details first, before its mid-level structures, and leaving the highest-level overall structure to last.

`Shift-reduce parser`: A class of efficient, table-driven bottom-up parsing methods for computer languages and other notations formally defined by a grammar.

`LR (Left-to-Right) Parser`: A type of bottom-up parser that analyses deterministic context-free languages in linear time.

#### The Abstract Syntax Tree (AST)

The AST captures its behavior in such a way that it's easy for later stages of the compiler (e.g code generation) to interpret. There should be one object for each construct in the language, and the AST should closely model the language.

In `ForTheSakeOfIt`, there are expressions, a prototype and a function object.

```c++
// ExprAST - Base class for all expression nodes.
class ExprAST {
    public:
    	virtual ~ExprAST() {}
};

// NumberExprAST - Expression class for numeric literals like 1.0.
class NumberExprAST : public ExprAST {
    double Val;
    
    public:
    	NumberExprAST(double Val): Val(Val) {}
};

// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
    std::string Name;
    
    public:
    	VariableExprAST(const std::string &Name) : Name(Name) {}
};

// BinaryExprAST - Expression class for a binary operator
class BinaryExprAST : public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
    
    public:
    	BinaryExprAST(char op, std::unique_ptr<ExprAST> LSH, std::unique_ptr<ExprAST> RHS) : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

// CallExprAST - Expression class for function calls
class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
    
    public:
    	CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args) : Callee(Callee), Args(std::move(Args)) {}
};

// PrototypeAST - This calss represents the 'prototype' for a function, which captures its name, and its argument names (thus implicitly the number of arguments the function takes).
class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;
    
    public:
    	PrototypeAST(const std::string &name, std::vector<std::string> Args) : Name(name), Args(std::move(Args)) {}
    const std::string &getName() const { return Name; }
};

// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
   
  public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body) : Proto(std::move(Proto)), Body(std::move(Body)) {}
};
```

Variables capture the variable name, binary operators capture their opcode (e.g '+') and calls capture a function name as well as a list of any argument expressions.

The AST captures the language features without talking about the syntax of the language.

In `ForTheSakeOfIt`, functions are typed with just a count of their arguments. Since all values are double precision floating point, the type of each argument doesn't need to be stored anywhere. In a more aggressive and realistic language, the 'ExprAST' class would probably have a type field.

#### Parser Basics

The parser that needs to be built should parse "x + y" into an AST that could be generated with calls like:

```c++
auto LHS = std::make_unique<VariableExprAST>("x");
auto RHS = std::make_unique<VariableExprAST>("y");
auto Result = std::make_unique<BinaryExprAST>('+', std::move(LHS), std::move(RHS));
```

Some basic helper routines to help generate such commands:

```c++
// CurTok/getNextToken - Provide a simple token buffer. Curtok is the current token the parser is looking at. getNextToken reads another token from the lexer and updates CurTok with its results

static int CurTok;
static int getNextToken() {
    return CurTok  gettok();
}
```

This implements a simple token buffer around the lexer which allows us to get a token ahead at what the lexer is returning. Every function in this parser will assume thta CurTok is the current token that needs to be parsed.

```c++
// LogError* - Helper functions for error handling
std::unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "LogError: %s\n", str);
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogError(Str);
    return nullptr;
}
```

The LogError routines are simple helper routines that the parser will use to handle errors. The error recovery in the parser will only be enough for the tutorial.



#### Basic Expression Parsing

For each production in `ForTheSakeOfIt's` grammar, a function is defined to parse that production. For numeric literals:

```c++
// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // Consume the number
    return std::move(Result);
}
```

This routine is called when the current token is a `tok_number`. It takes the current number value, creates a `NumberExprAST` node, advances the lexer to the next token, and finally returns.

This routine eats all of the tokens that correspond to the production and returns the lexer buffer with the next token ready to go. A parenthesis operator for a recursive descent parser is as follows:

```c++
// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // consume (.
    auto V = ParseExpression();
    if (!V)
        return nullptr;
    if (CurTok != ')')
        return LogError("expected ')'");
    getNextToken(); // consume ).
    return V
}
```

`ParseParenExpr`:

1. Shows how to use the `LogError` routines. Used when an error occurs
2. Uses recursion by calling `ParseExpression` which allows for handling of recursive grammars and keeps each production very simple.

```c++
// identifierexpr
// ::= identifier
// ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;
    
    getNextToken(); // consume identifier
    
    if (CurTok != '(') // Simple variable reference
        return std::make_unique<VariableExprAST>(IdName);
    
    // Call
    getNextToken(); // consume (
    std::vector<std::unique_ptr<ExprAST>> Args;
    
    if (CurTok != ')') {
        while(1) {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;
            
            if (CurTok == ')')
                break;
            
            if (CurTok != ')')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    
    // Consume ')'
    getNextToken();
    
    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}
```

This routine uses look-ahead to determine if the current identifier is a standalone variable reference or it it's a function call expression. This is done by checking to see if the token after the identifier is a '(' token, constructing either a `VariableExprAST` or `CallExprAST` node as appropriate.

The following helper function wraps everything together into one entry point.

```c++
// primary
// ::= identifier
// ::= numberexpr
// ::= parenexpr

static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        default:
            return LogError("Unknown token when expecting an expression");
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
    }
}
```



#### Binary Expression Parsing

These are significantly harder to parse because they are often ambiguous. `Operator-Precedence Parsing` is used in this scenario so that the parsing recursion is guided by the precedence of binary operators.

The following is a *table of precedencies*:

```c++
// BinopPrecendence - Holds the precedence for each binary operator that's defined
static std::map<char, int> BinopPrecedence;

// GetTokPrecedence - Get the precedence of the pending binary operator token
static int GetTokPrecedence() {
    if (!isascii(CurTok))
        return -1;
    
    // Make sure it's a declared binop
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) return -1;
    return TokPrec;
}

int main() {
    // Install standard binary operators.
    // 1 is lowest precedence
    BinopPrecendence['<'] = 10;
    BinopPrecendence['+'] = 20;
    BinopPrecendence['-'] = 20;
    BinopPrecendence['*'] = 40;
    ...
}
```

`ForTheSakeOfIt` will only support 4 binary operators which can be extended. The `GetTokPrecendence` function returns the precedence for the current token, or -1 if the token in not a binary operator.

```c++
// expression
// ::= primary binoprphs
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS)
        return nullptr;
    return ParseBinOpRHS(0, std::move(LHS));
}
```

`ParseBinOpRHS` is the function that parses the sequence of pairs for us. It takes a precedence and a pointer to an expression for the part that has been parsed so far. The precedence value passed into `ParseBinOpRHS` indicates the `minimal operator precendence` that the function is allowed to consume. 

```c++
// binoprhs
// ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
    // If this is a binop, find it's precedence
    while(1) {
        int TokPrec = GetTokPrecedence();
        
        // If this is a binop that binds at least as tightly as the current binop, consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;
```

This code gets the precedence of the current token and checks to see it it's too low. If the check succeeds, we know that the token is a binary operator and that it will be included in this expression:

```c++
    int BinOp = CurTok;
    getNextToken(); // Consume binop

    // Parse the primary expression after the binary operator
    auto RHS = ParsePrimary();
    if (!RHS)
        return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let the pending operator take RHS as its LHS
    int NextPrec = GetTokPrecedence();
    // If the precedence of the binop to the RHS is lower or equal to the precedence of our current operator, then the parenthesis associates as "(a+b) binop ..."
    if (TokPrec < NextPrec) {}
    // Merge LHS/RHS
    LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    } 
}
```

#### Parsing Function Prototypes

In `ForTheSakeOfIt`, these are used for both `extern` function declarations as well as function body definitions.

```c++
// Prototype ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function nane in prototype");
    
    std::string FnName = IdentifierStr;
    getNextToken();
    
    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");
    
    // Read the list of argument names.
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);
    if (CurTok != ')')
        return LogErrorP("Exptected ')' in prototype");
   
    // success
    getNextToken();
    
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}
```

```c++
// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken();
    auto Proto = ParsePrototype();
    if (!Proto) return nullptr;
    
    if (auto E = ParseExpression())
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

// external ::= 'extern' prototype
static std::unique_ptr<Prototype> ParseExtern() {
    getNextToken();
    return ParsePrototype();
}
```

Finally, the user can type in arbitrary top-level expressions and evaluate them on the fly. This anonymous nullary function does that.

```c++
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto E = ParseExpression()) {
        auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}
```

### The Driver

This part simply invokes all of the parsing pieces with a top-level dispatch loop.

```c++
// driver
static void MainLoop() {
    while(1) {
        fprintf(stderr, "ready> ");
        switch (CurTok) {
            case tok_eof:
                return;
            case ';':
                getNextToken();
                break;
            case tok_def:
                HandleDefinition();
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                HandleTopLevelExpression();
                break;
                
        }
    }
}
```

