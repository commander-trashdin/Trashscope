# A Somewhat Modern Version Of LLVM Tutorial (SMVLT)

Based on the "My First Language Frontend with LLVM" tutorial by the LLVM Project.

## Table of Contents

- [Introduction](#introduction)
- [Chapters](#chapters)
  - [Chapter 1: Kaleidoscope Language and Lexer](#chapter-1-kaleidoscope-language-and-lexer)
  - [Chapter 2: Implementing a Parser and AST](#chapter-2-implementing-a-parser-and-ast)
  - [Chapter 3: Code Generation to LLVM IR](#chapter-3-code-generation-to-llvm-ir)
  - [Chapter 5: Extending the Language - Control Flow](#chapter-5-extending-the-language---control-flow)
  - [Chapter 7: Extending the Language - Mutable Variables](#chapter-7-extending-the-language---mutable-variables)
  - [Chapter 8: Compiling to Object Files](#chapter-8-compiling-to-object-files)
- [Installation](#installation)
- [Contributing](#contributing)
- [Acknowledgments](#acknowledgments)

## Introduction

This project aims to:

1. **Update the Code**: Modify the original tutorial code so that it compiles with current versions of LLVM, removing deprecated functions and legacy constructs.

2. **Improve Structure**: Slightly adjust the code structure by separating it into multiple files. This enhances readability and maintainability without deviating significantly from the original tutorial.

**Note**: The goal is to make minimal improvements necessary for the code to function correctly and be easier to work with. This is not a complete overhaul but rather an incremental update to help learners follow along with modern tooling.


## Chapters

### Chapter 1: Kaleidoscope Language and Lexer
=============================

*In this chapter, we introduce the Kaleidoscope language and implement a simple lexer in C++. The lexer will tokenize the input stream into meaningful symbols for parsing.*

#### 1.1 The Basic Language

This tutorial is illustrated with a toy language called “Kaleidoscope” (derived from “meaning beautiful, form, and view”). Kaleidoscope is a procedural language that allows you to define functions, use conditionals, math, etc. Over the course of the tutorial, we’ll extend Kaleidoscope to support the if/then/else construct, a for loop, user defined operators, JIT compilation with a simple command line interface, debug info, etc.

We want to keep things simple, so the only datatype in Kaleidoscope is a 64-bit floating point type (aka ‘double’ in C parlance). As such, all values are implicitly double precision and the language doesn’t require type declarations. This gives the language a very nice and simple syntax. For example, the following simple example computes Fibonacci numbers:

```
# Compute the x'th fibonacci number.
def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2)

# This expression will compute the 40th number.
fib(40)
```

We also allow Kaleidoscope to call into standard library functions (the LLVM JIT makes this completely trivial). This means that you can use the `extern` keyword to define a function before you use it (this is also useful for mutually recursive functions). For example:

```
extern sin(arg);
extern cos(arg);
extern atan2(arg1 arg2);

atan2(sin(.4), cos(42))
```

Lets dive into the implementation of this language!

#### 1.2 The Lexer
When it comes to implementing a language, the first thing needed is the ability to process a text file and recognize what it says. The traditional way to do this is to use a “lexer” (aka ‘scanner’) to break the input up into “tokens”. Each token returned by the lexer includes a token code and potentially some metadata (e.g. the numeric value of a number). Here is a sketch of the Tokenizer class:

```c++
class Tokenizer {
public:
// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
 enum TType {
    Eof = -1,

    // commands
    Def = -2,
    Extern = -3,

    // primary
    Identifier = -4,
    Number = -5,
  };
...
private:
  int LastChar = ' ';
  std::string IdentifierStr; // Filled in if Identifier
  double NumVal;             // Filled in if Number
  int CurTok;
  std::map<char, int> BinopPrecedence;
};
```


Each token returned by our lexer will either be one of the Token enum values or it will be an ‘unknown’ character like ‘+’, which is returned as its ASCII value. If the current token is an identifier, the IdentifierStr global variable holds the name of the identifier. If the current token is a numeric literal (like 1.0), NumVal holds its value. We use global variables for simplicity, but this is not the best choice for a real language implementation :).

The actual implementation of the lexer is a single function named gettok. The gettok function is called to return the next token from standard input. Its definition starts as:

```c++
int Tokenizer::GetTok() {
  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();
```

It works by calling the C getchar() function to read characters one at a time from standard input. It eats them as it recognizes them and stores the last character read, but not processed, in LastChar. The first thing that it has to do is ignore whitespace between tokens. This is accomplished with the loop above.

The next thing GetTok needs to do is recognize identifiers and specific keywords like `def`. Kaleidoscope does this with this simple loop:

```c++
if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;

    if (IdentifierStr == "def")
      return Def;
    if (IdentifierStr == "extern")
      return Extern;
    return Identifier;
  }
```

Note that this code sets the `IdentifierStr` member whenever it lexes an identifier. Also, since language keywords are matched by the same loop, we handle them here inline. Numeric values are similar:

```c++
if (isdigit(LastChar) || LastChar == '.') {   // Number: [0-9.]+
  std::string NumStr;
  do {
    NumStr += LastChar;
    LastChar = getchar();
  } while (isdigit(LastChar) || LastChar == '.');

  NumVal = strtod(NumStr.c_str(), 0);
  return tok_number;
}
```

This is all pretty straightforward code for processing input. When reading a numeric value from input, we use the C strtod function to convert it to a numeric value that we store in NumVal. Note that this isn’t doing sufficient error checking: it will incorrectly read “1.23.45.67” and handle it as if you typed in “1.23”. Feel free to extend it! Next we handle comments:

```c++
if (LastChar == '#') {
    // Comment until end of line.
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return GetTok();
  }
```

We handle comments by skipping to the end of the line and then return the next token. Finally, if the input doesn’t match one of the above cases, it is either an operator character like `+` or the end of the file. These are handled with this code:

```c++
  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return Eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}
```

With this, we have the complete lexer for the basic Kaleidoscope language (the full code listing for the Lexer is available in the next chapter of the tutorial). Next we’ll build a simple parser that uses this to build an Abstract Syntax Tree. When we have that, we’ll include a driver so that you can use the lexer and parser together.

### Chapter 2: Implementing a Parser and AST
=============================

*Here, we build upon the lexer to create a parser that constructs an Abstract Syntax Tree (AST). We discuss recursive descent parsing and operator precedence parsing techniques.*

Welcome to Chapter 2 of the “Implementing a language with LLVM” tutorial. This chapter shows you how to use the lexer, built in Chapter 1, to build a full parser for our Kaleidoscope language. Once we have a parser, we’ll define and build an Abstract Syntax Tree (AST).

The parser we will build uses a combination of Recursive Descent Parsing and Operator-Precedence Parsing to parse the Kaleidoscope language (the latter for binary expressions and the former for everything else). Before we get to parsing though, let’s talk about the output of the parser: the Abstract Syntax Tree.


#### 2.1 The Abstract Syntax Tree (AST)

The AST for a program captures its behavior in such a way that it is easy for later stages of the compiler (e.g. code generation) to interpret. We basically want one object for each construct in the language, and the AST should closely model the language. In Kaleidoscope, we have expressions, a prototype, and a function object. We’ll start with expressions first:

```c++
/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
  virtual llvm::Value *codegen() = 0;
};


/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  llvm::Value *codegen() override;
};
```

The code above shows the definition of the base ExprAST class and one subclass which we use for numeric literals. The important thing to note about this code is that the NumberExprAST class captures the numeric value of the literal as an instance variable. This allows later phases of the compiler to know what the stored numeric value is.

Right now we only create the AST, so there are no useful accessor methods on them. It would be very easy to add a virtual method to pretty print the code, for example. Here are the other expression AST node definitions that we'll use in the basic form of the Kaleidoscope language:

```c++
/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
  llvm::Value *codegen() override;

  const std::string &getName() const { return Name; }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  llvm::Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}
  llvm::Value *codegen() override;
};
```

This is all (intentionally) rather straight-forward: variables capture the variable name, binary operators capture their opcode (e.g. `+`), and calls capture a function name as well as a list of any argument expressions. One thing that is nice about our AST is that it captures the language features without talking about the syntax of the language. Note that there is no discussion about precedence of binary operators, lexical structure, etc.

For our basic language, these are all of the expression nodes we'll define. Because it doesn't have conditional control flow, it isn't Turing-complete; we'll fix that in a later installment. The two things we need next are a way to talk about the interface to a function, and a way to talk about functions themselves:

```c++
/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
      : Name(Name), Args(std::move(Args)) {}

  const std::string &getName() const { return Name; }
  llvm::Function *codegen();
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}
  llvm::Function *codegen();
};
```

In Kaleidoscope, functions are typed with just a count of their arguments. Since all values are double precision floating point, the type of each argument doesn't need to be stored anywhere. In a more aggressive and realistic language, the `ExprAST` class would probably have a type field.

With this scaffolding, we can now talk about parsing expressions and function bodies in Kaleidoscope.


#### 2.2 Parser Basics

Note -- the design for the tokenizer is different from the original. It is a singleton class with methods (instead of functions and a global). 

```c++
// The names (as by usual convetion) should probably be more like curTok_ -- feel free to adjust them as well
class Tokenizer {
  // The lexer returns tokens [0-255] if it is an unknown character, otherwise
  // one of these for known things.
public:
  static std::unique_ptr<Tokenizer> &GetInstance();

protected:
  Tokenizer() { GetNextToken(); }

  static std::unique_ptr<Tokenizer> singleton_;

private:
  int LastChar = ' ';
  std::string IdentifierStr; // Filled in if tok_identifier
  double NumVal;             // Filled in if tok_number
  int CurTok;
  std::map<char, int> BinopPrecedence; // This holds the precedence for each binary operator that is defined
};
```

Now that we have an AST to build, we need to define the parser code to build it. The idea here is that we want to parse something like `x+y` (which is returned as three tokens by the lexer) into an AST that could be generated with calls like this:

```c++
ExprAST *X = new VariableExprAST("x");
ExprAST *Y = new VariableExprAST("y");
ExprAST *Result = new BinaryExprAST('+', X, Y);
```
In order to do this, we'll start by defining some basic helper routines:

```c++
/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
int GetNextToken() { return CurTok = GetTok(); }
```

This implements a simple token buffer around the lexer. This allows us to look one token ahead at what the lexer is returning. Every function in our parser will assume that CurTok is the current token that needs to be parsed.

```c++
/// Error* - These are little helper functions for error handling. See errors.h/cpp for reference
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}
```

The Error routines are simple helper routines that our parser will use to handle errors. The error recovery in our parser will not be the best (it is actually pretty bad) and is not particular user-friendly, but it will be enough for our tutorial. These routines make it easier to handle errors in routines that have various return types: they always return null.

With these basic helper functions, we can implement the first piece of our grammar: numeric literals.


#### 2.3 Basic Expression Parsing

We start with numeric literals, because they are the simplest to process. For each production in our grammar, we'll define a function which parses that production. For numeric literals, we have:

```c++
/// numberexpr ::= number
std::unique_ptr<ExprAST> Tokenizer::ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  GetNextToken(); // consume the number
  return std::move(Result);
}
```

This routine is very simple: it expects to be called when the current token is a `Number` token. It takes the current number value, creates a `NumberExprAST` node, advances the lexer to the next token, and finally returns.

There are some interesting aspects to this. The most important one is that this routine eats all of the tokens that correspond to the production and returns the lexer buffer with the next token (which is not part of the grammar production) ready to go. This is a fairly standard way to go for recursive descent parsers. For a better example, the parenthesis operator is defined like this:

```c++
std::unique_ptr<ExprAST> Tokenizer::ParseParenExpr() {
  GetNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("expected ')'");
  GetNextToken(); // eat ).
  return V;
}
```

This function illustrates a number of interesting things about the parser:

1) It shows how we use the Error routines. When called, this function expects that the current token is a `(` token, but after parsing the subexpression, it is possible that there is no `)` waiting. For example, if the user types in `(4 x` instead of `(4)`, the parser should emit an error. Because errors can occur, the parser needs a way to indicate that they happened: in our parser, we return null on an error.

2) Another interesting aspect of this function is that it uses recursion by calling ParseExpression (we will soon see that ParseExpression can call ParseParenExpr). This is powerful because it allows us to handle recursive grammars, and keeps each production very simple. Note that parentheses do not cause construction of AST nodes themselves. While we could do it this way, the most important role of parentheses are to guide the parser and provide grouping. Once the parser constructs the AST, parentheses are not needed.

The next simple production is for handling variable references and function calls:

```c++
/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
std::unique_ptr<ExprAST> Tokenizer::ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  GetNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  GetNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      GetNextToken();
    }
  }

  // Eat the ')'.
  GetNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}
```

This routine follows the same style as the other routines. (It expects to be called if the current token is a tok_identifier token). It also has recursion and error handling. One interesting aspect of this is that it uses look-ahead to determine if the current identifier is a stand alone variable reference or if it is a function call expression. It handles this by checking to see if the token after the identifier is a `(` token, constructing either a `VariableExprAST` or `CallExprAST` node as appropriate.

Now that we have all of our simple expression-parsing logic in place, we can define a helper function to wrap it together into one entry point. We call this class of expressions "primary" expressions, for reasons that will become more clear later in the tutorial. In order to parse an arbitrary primary expression, we need to determine what sort of expression it is:

```c++
/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
std::unique_ptr<ExprAST> Tokenizer::ParsePrimary() {
  switch (CurTok) {
  case Identifier:
    return ParseIdentifierExpr();
  case Number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  default:
    return LogError("unknown token when expecting an expression");
  }
}
```

Now that you see the definition of this function, it is more obvious why we can assume the state of `CurTok` in the various functions. This uses look-ahead to determine which sort of expression is being inspected, and then parses it with a function call.

Now that basic expressions are handled, we need to handle binary expressions. They are a bit more complex.


#### 2.4 Binary Expression Parsing

Binary expressions are significantly harder to parse because they are often ambiguous. For example, when given the string `x+y*z`, the parser can choose to parse it as either `(x+y)*z` or `x+(y*z)`. With common definitions from mathematics, we expect the later parse, because `*` (multiplication) has higher precedence than `+` (addition).

There are many ways to handle this, but an elegant and efficient way is to use Operator-Precedence Parsing. This parsing technique uses the precedence of binary operators to guide recursion. To start with, we need a table of precedences:

```c++
/// GetTokPrecedence - Get the precedence of the pending binary operator token.
int Tokenizer::GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  if (auto search = BinopPrecedence.find(CurTok);
      search != BinopPrecedence.end()) {
    return search->second;
  }
  return -1;
}


int &operator[](std::size_t idx) { return BinopPrecedence[idx]; }


int main() {
  auto tok = *Tokenizer::GetInstance();
  // Install standard binary operators.
  // 1 is lowest precedence.
  tok['='] = 2;
  tok['<'] = 10;
  tok['+'] = 20;
  tok['-'] = 20;
  tok['*'] = 40; // highest.
  ...
}
```

For the basic form of Kaleidoscope, we will only support 4 binary operators (this can obviously be extended by you, our brave and intrepid reader). The `GetTokPrecedence` function returns the precedence for the current token, or -1 if the token is not a binary operator. Having a map makes it easy to add new operators and makes it clear that the algorithm doesn't depend on the specific operators involved, but it would be easy enough to eliminate the map and do the comparisons in the `GetTokPrecedence` function. (Or just use a fixed-size array).

With the helper above defined, we can now start parsing binary expressions. The basic idea of operator precedence parsing is to break down an expression with potentially ambiguous binary operators into pieces. Consider, for example, the expression `a+b+(c+d)*e*f+g`. Operator precedence parsing considers this as a stream of primary expressions separated by binary operators. As such, it will first parse the leading primary expression `a`, then it will see the pairs `[+, b] [+, (c+d)] [\*, e] [\*, f]` and `[+, g]`. Note that because parentheses are primary expressions, the binary expression parser doesn't need to worry about nested subexpressions like `(c+d)` at all.

To start, an expression is a primary expression potentially followed by a sequence of [binop,primaryexpr] pairs:

```c++
/// expression
///   ::= primary binoprhs
///
std::unique_ptr<ExprAST> Tokenizer::ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}
```

`ParseBinOpRHS` is the function that parses the sequence of pairs for us. It takes a precedence and a pointer to an expression for the part that has been parsed so far. Note that `x` is a perfectly valid expression: As such, `binoprhs` is allowed to be empty, in which case it returns the expression that is passed into it. In our example above, the code passes the expression for `a` into ParseBinOpRHS and the current token is `+`.

The precedence value passed into ParseBinOpRHS indicates the minimal operator precedence that the function is allowed to eat. For example, if the current pair stream is `[+, x]` and `ParseBinOpRHS` is passed in a precedence of 40, it will not consume any tokens (because the precedence of `+` is only 20). With this in mind, `ParseBinOpRHS` starts with:

```c++
/// binoprhs
///   ::= ('+' primary)*
std::unique_ptr<ExprAST>
Tokenizer::ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;
```

This code gets the precedence of the current token and checks to see if if is too low. Because we defined invalid tokens to have a precedence of -1, this check implicitly knows that the pair-stream ends when the token stream runs out of binary operators. If this check succeeds, we know that the token is a binary operator and that it will be included in this expression:

```c++
    // Okay, we know this is a binop.
    int BinOp = CurTok;
    GetNextToken(); // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;
```

As such, this code eats (and remembers) the binary operator and then parses the primary expression that follows. This builds up the whole pair, the first of which is `[+, b]` for the running example.

Now that we parsed the left-hand side of an expression and one pair of the RHS sequence, we have to decide which way the expression associates. In particular, we could have `(a+b) binop unparsed` or `a + (b binop unparsed)`. To determine this, we look ahead at `binop` to determine its precedence and compare it to BinOp's precedence (which is `+` in this case):

```c++
// If BinOp binds less tightly with RHS than the operator after RHS, let
// the pending operator take RHS as its LHS.
int NextPrec = GetTokPrecedence();
if (TokPrec < NextPrec) {
```

If the precedence of the binop to the right of "RHS" is lower or equal to the precedence of our current operator, then we know that the parentheses associate as `(a+b) binop ...`. In our example, the current operator is `+` and the next operator is `+`, we know that they have the same precedence. In this case we'll create the AST node for `a+b`, and then continue parsing:

```c++
      ... if body omitted ...
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }  // loop around to the top of the while loop.
}
```

In our example above, this will turn `a+b+` into `(a+b)` and execute the next iteration of the loop, with `+` as the current token. The code above will eat, remember, and parse `(c+d)` as the primary expression, which makes the current pair equal to [+, (c+d)]. It will then evaluate the `if` conditional above with `*` as the binop to the right of the primary. In this case, the precedence of `*` is higher than the precedence of `+` so the if condition will be entered.

The critical question left here is "how can the if condition parse the right hand side in full"? In particular, to build the AST correctly for our example, it needs to get all of `(c+d)*e*f` as the RHS expression variable. The code to do this is surprisingly simple (code from the above two blocks duplicated for context):

```c++
    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }
    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }  // loop around to the top of the while loop.
}
```

At this point, we know that the binary operator to the `RHS` of our primary has higher precedence than the binop we are currently parsing. As such, we know that any sequence of pairs whose operators are all higher precedence than `+` should be parsed together and returned as "RHS". To do this, we recursively invoke the `ParseBinOpRHS` function specifying "TokPrec+1" as the minimum precedence required for it to continue. In our example above, this will cause it to return the AST node for `(c+d)*e*f` as RHS, which is then set as the RHS of the `+` expression.

Finally, on the next iteration of the while loop, the `+g` piece is parsed and added to the AST. With this little bit of code (14 non-trivial lines), we correctly handle fully general binary expression parsing in a very elegant way. This was a whirlwind tour of this code, and it is somewhat subtle. I recommend running through it with a few tough examples to see how it works.

This wraps up handling of expressions. At this point, we can point the parser at an arbitrary token stream and build an expression from it, stopping at the first token that is not part of the expression. Next up we need to handle function definitions, etc.


#### 2.5 Parsing the Rest

The next thing missing is handling of function prototypes. In Kaleidoscope, these are used both for `extern` function declarations as well as function body definitions. The code to do this is straight-forward and not very interesting (once you've survived expressions):

```c++
/// prototype
///   ::= id '(' id* ')'
std::unique_ptr<PrototypeAST> Tokenizer::ParsePrototype() {
  if (CurTok != Identifier)
    return LogErrorP("Expected function name in prototype");

  std::string FnName = IdentifierStr;
  GetNextToken();

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  // Read the list of argument names.
  std::vector<std::string> ArgNames;
  while (GetNextToken() == Identifier)
    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // success.
  GetNextToken(); // eat ')'.

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}
```

Given this, a function definition is very simple, just a prototype plus an expression to implement the body:

```c++
/// definition ::= 'def' prototype expression
std::unique_ptr<FunctionAST> Tokenizer::ParseDefinition() {
  GetNextToken(); // eat def.
  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}
```

In addition, we support `extern` to declare functions like `sin` and `cos` as well as to support forward declaration of user functions. These ‘extern's are just prototypes with no body:

```c++
/// external ::= 'extern' prototype
std::unique_ptr<PrototypeAST> Tokenizer::ParseExtern() {
  GetNextToken(); // eat extern.
  return ParsePrototype();
}
```

Finally, we'll also let the user type in arbitrary top-level expressions and evaluate them on the fly. We will handle this by defining anonymous nullary (zero argument) functions for them:

```c++
/// toplevelexpr ::= expression
std::unique_ptr<FunctionAST> Tokenizer::ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}
```

Now that we have all the pieces, let's build a little driver that will let us actually execute this code we've built!


#### 2.6 The Driver

The driver for this simply invokes all of the parsing pieces with a top-level dispatch loop. There isn't much interesting here, so I'll just include the top-level loop. See below for full code in the "Top-Level Parsing" section.

```c++
/// top ::= definition | external | expression | ';'
void Tokenizer::MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case Eof:
      return;
    case ';': // ignore top-level semicolons.
      GetNextToken();
      break;
    case Def:
      HandleDefinition();
      break;
    case Extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

```

The most interesting part of this is that we ignore top-level semicolons. Why is this, you ask? The basic reason is that if you type `4 + 5` at the command line, the parser doesn't know whether that is the end of what you will type or not. For example, on the next line you could type `def foo...` in which case `4+5` is the end of a top-level expression. Alternatively you could type `* 6`, which would continue the expression. Having top-level semicolons allows you to type `4+5;`, and the parser will know you are done.

#### 2.8. Conclusions
With just under 400 lines of commented code (240 lines of non-comment, non-blank code), we fully defined our minimal language, including a lexer, parser, and AST builder. With this done, the executable will validate Kaleidoscope code and tell us if it is grammatically invalid. For example, here is a sample interaction:

```
$ ./a.out
ready> def foo(x y) x+foo(y, 4.0);
Parsed a function definition.
ready> def foo(x y) x+y y;
Parsed a function definition.
Parsed a top-level expr
ready> def foo(x y) x+y );
Parsed a function definition.
Error: unknown token when expecting an expression
ready> extern sin(a);
ready> Parsed an extern
ready> ^D
$
```

There is a lot of room for extension here. You can define new AST nodes, extend the language in many ways, etc. In the next installment, we will describe how to generate LLVM Intermediate Representation (IR) from the AST.

### Chapter 3: Code Generation to LLVM IR
=============================

*This chapter covers generating LLVM Intermediate Representation (IR) from the AST. We integrate LLVM into the project to facilitate code generation.*

#### 3.1. Introduction
Welcome to Chapter 3 of the “Implementing a language with LLVM” tutorial. This chapter shows you how to transform the Abstract Syntax Tree, built in Chapter 2, into LLVM IR. This will teach you a little bit about how LLVM does things, as well as demonstrate how easy it is to use. It’s much more work to build a lexer and parser than it is to generate LLVM IR code. :)

Please note: the code in this chapter and later require LLVM 3.7 or later. LLVM 3.6 and before will not work with it. Also note that you need to use a version of this tutorial that matches your LLVM release: If you are using an official LLVM release, use the version of the documentation included with your release or on the llvm.org releases page.

#### 3.2. Code Generation Setup

Important note -- in the original tutorial, most the the llvm objects required globally for codegen, where made statis globals. Which is generally a _really bad idea_. Simply to avoid that here we will have the following base class (with a somewhat random name, feel free to rename it, as well as anything else):

```c++
class AST {
public:
  static llvm::LLVMContext &getContext() {
    static std::unique_ptr<llvm::LLVMContext> TheContext =
        std::make_unique<llvm::LLVMContext>();
    return getContext();
  }

  static llvm::IRBuilder<> &getBuilder() {
    static std::unique_ptr<llvm::IRBuilder<>> Builder =
        std::make_unique<llvm::IRBuilder<>>(getContext());
    return *Builder;
  }

  static llvm::Module &getModule() {
    static std::unique_ptr<llvm::Module> TheModule =
        std::make_unique<llvm::Module>("my cool jit", getContext());
    return *TheModule;
  }

  static std::map<std::string, llvm::AllocaInst *> &getNamedValues() {
    static std::map<std::string, llvm::AllocaInst *> NamedValues;
    return NamedValues;
  }
};
```
These will be used during code generation. `Context` is an opaque object that owns a lot of core LLVM data structures, such as the type and constant value tables. We don’t need to understand it in detail, we just need a single instance to pass into APIs that require it.

The `Builder` object is a helper object that makes it easy to generate LLVM instructions. Instances of the `IRBuilder` class template keep track of the current place to insert instructions and has methods to create new instructions.

`Module` is an LLVM construct that contains functions and global variables. In many ways, it is the top-level structure that the LLVM IR uses to contain code. It will own the memory for all of the IR that we generate, which is why the `codegen()` method returns a raw `llvm::Value*`, rather than a `std::unique_ptr<llvm::Value>`.

The `NamedValues` map keeps track of which values are defined in the current scope and what their LLVM representation is. (In other words, it is a symbol table for the code). In this form of Kaleidoscope, the only things that can be referenced are function parameters. As such, function parameters will be in this map when generating code for their function body.

This will allow all classes below direct access to the objects needed. There is no interesting reason why should this be inherited either, it just was. Now, back to the tutorial.

In order to generate LLVM IR, we want some simple setup to get started. First we define virtual code generation (codegen) methods in each AST class:

```c++
/// ExprAST - Base class for all expression nodes.
class ExprAST : public AST {
public:
  virtual ~ExprAST() = default;
  virtual Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  Value *codegen() override;
};
...
```

The `codegen()` method says to emit IR for that AST node along with all the things it depends on, and they all return an LLVM Value object. “Value” is the class used to represent a “Static Single Assignment (SSA) register” or “SSA value” in LLVM. The most distinct aspect of SSA values is that their value is computed as the related instruction executes, and it does not get a new value until (and if) the instruction re-executes. In other words, there is no way to “change” an SSA value. For more information, please read up on Static Single Assignment - the concepts are really quite natural once you grok them.

Note that instead of adding virtual methods to the `ExprAST` class hierarchy, it could also make sense to use a visitor pattern or some other way to model this. Again, this tutorial won’t dwell on good software engineering practices: for our purposes, adding a virtual method is simplest.

The second thing we want is a “LogError” method like we used for the parser, which will be used to report errors found during code generation (for example, use of an undeclared parameter):

```c++
llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}
```

With these basics in place, we can start talking about how to generate code for each expression. Note that this assumes that the Builder has been set up to generate code into something. For now, we’ll assume that this has already been done, and we’ll just use it to emit code.

#### 3.3. Expression Code Generation

Generating LLVM code for expression nodes is very straightforward: less than 45 lines of commented code for all four of our expression nodes. First we’ll do numeric literals:

```c++
llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(getContext(), llvm::APFloat(Val));
}
```

In the LLVM IR, numeric constants are represented with the ConstantFP class, which holds the numeric value in an `APFloat` internally (`APFloat` has the capability of holding floating point constants of Arbitrary Precision). This code basically just creates and returns a `ConstantFP`. Note that in the LLVM IR that constants are all uniqued together and shared. For this reason, the API uses the `foo::get(…)` idiom instead of `new foo(..)` or `foo::Create(..)`.

```c++
llvm::Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  llvm::Value *V = getNamedValues()[Name];
  if (!V)
    LogErrorV("Unknown variable name");
  return V;
}
```

References to variables are also quite simple using LLVM. In the simple version of Kaleidoscope, we assume that the variable has already been emitted somewhere and its value is available. In practice, the only values that can be in the `NamedValues` map are function arguments. This code simply checks to see that the specified name is in the map (if not, an unknown variable is being referenced) and returns the value for it. In future chapters, we’ll add support for loop induction variables in the symbol table, and for local variables.

```c++
llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return getBuilder().CreateFAdd(L, R, "addtmp");
  case '-':
    return getBuilder().CreateFSub(L, R, "subtmp");
  case '*':
    return getBuilder().CreateFMul(L, R, "multmp");
  case '<':
    L = getBuilder().CreateFCmpULT(L, R, "cmptmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return getBuilder().CreateUIToFP(
        L, llvm::Type::getDoubleTy(getContext()), "booltmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}
```

Binary operators start to get more interesting. The basic idea here is that we recursively emit code for the left-hand side of the expression, then the right-hand side, then we compute the result of the binary expression. In this code, we do a simple switch on the opcode to create the right LLVM instruction.

In the example above, the LLVM builder class is starting to show its value. `IRBuilder` knows where to insert the newly created instruction, all you have to do is specify what instruction to create (e.g. with `CreateFAdd`), which operands to use (`L`and `R` here) and optionally provide a name for the generated instruction.

One nice thing about LLVM is that the name is just a hint. For instance, if the code above emits multiple “addtmp” variables, LLVM will automatically provide each one with an increasing, unique numeric suffix. Local value names for instructions are purely optional, but it makes it much easier to read the IR dumps.

LLVM instructions are constrained by strict rules: for example, the Left and Right operands of an add instruction must have the same type, and the result type of the add must match the operand types. Because all values in Kaleidoscope are doubles, this makes for very simple code for add, sub and mul.

On the other hand, LLVM specifies that the fcmp instruction always returns an `i1` value (a one bit integer). The problem with this is that Kaleidoscope wants the value to be a 0.0 or 1.0 value. In order to get these semantics, we combine the fcmp instruction with a uitofp instruction. This instruction converts its input integer into a floating point value by treating the input as an unsigned value. In contrast, if we used the sitofp instruction, the Kaleidoscope ‘<’ operator would return 0.0 and -1.0, depending on the input value.

```c++
llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  llvm::Function *CalleeF = getModule().getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<llvm::Value *> ArgsV;
  for (auto &Arg : Args) {
    ArgsV.push_back(Arg->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return getBuilder().CreateCall(CalleeF, ArgsV, "calltmp");
}
```

Code generation for function calls is quite straightforward with LLVM. The code above initially does a function name lookup in the LLVM Module’s symbol table. Recall that the LLVM Module is the container that holds the functions we are JIT’ing. By giving each function the same name as what the user specifies, we can use the LLVM symbol table to resolve function names for us.

Once we have the function to call, we recursively codegen each argument that is to be passed in, and create an LLVM call instruction. Note that LLVM uses the native C calling conventions by default, allowing these calls to also call into standard library functions like `sin` and `cos`, with no additional effort.

This wraps up our handling of the four basic expressions that we have so far in Kaleidoscope. Feel free to go in and add some more. For example, by browsing the LLVM language reference you’ll find several other interesting instructions that are really easy to plug into our basic framework.

#### 3.4. Function Code Generation

Code generation for prototypes and functions must handle a number of details, which make their code less beautiful than expression code generation, but allows us to illustrate some important points. First, let’s talk about code generation for prototypes: they are used both for function bodies and external function declarations. The code starts with:

```c++
llvm::Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<llvm::Type *> Doubles(Args.size(),
                                    llvm::Type::getDoubleTy(getContext()));
  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::Type::getDoubleTy(getContext()), Doubles, false);

  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, Name, getModule());
```

This code packs a lot of power into a few lines. Note first that this function returns a `Function*` instead of a `Value*`. Because a “prototype” really talks about the external interface for a function (not the value computed by an expression), it makes sense for it to return the LLVM Function it corresponds to when codegen’d.

The call to `FunctionType::get` creates the `FunctionType` that should be used for a given Prototype. Since all function arguments in Kaleidoscope are of type double, the first line creates a vector of “N” LLVM double types. It then uses the Functiontype::get method to create a function type that takes “N” doubles as arguments, returns one double as a result, and that is not vararg (the false parameter indicates this). Note that Types in LLVM are uniqued just like Constants are, so you don’t “new” a type, you “get” it.

The final line above actually creates the IR `Function` corresponding to the `Prototype`. This indicates the type, linkage and name to use, as well as which module to insert into. “external linkage” means that the function may be defined outside the current module and/or that it is callable by functions outside the module. The Name passed in is the name the user specified: since “TheModule” is specified, this name is registered in `Module`s symbol table.

```c++
  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}
```
Finally, we set the name of each of the function’s arguments according to the names given in the `Prototype`. This step isn’t strictly necessary, but keeping the names consistent makes the IR more readable, and allows subsequent code to refer directly to the arguments for their names, rather than having to look up them up in the Prototype AST.

At this point we have a function prototype with no body. This is how LLVM IR represents function declarations. For extern statements in Kaleidoscope, this is as far as we need to go. For function definitions however, we need to codegen and attach a function body.

```c++
llvm::Function *FunctionAST::codegen() {
    // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function *TheFunction = getModule().getFunction(Proto->getName());

  if (!TheFunction)
    TheFunction = Proto->codegen();

  if (!TheFunction)
    return nullptr;

  if (!TheFunction->empty())
    return (llvm::Function *)LogErrorV("Function cannot be redefined.");
```

For function definitions, we start by searching `Module`’s symbol table for an existing version of this function, in case one has already been created using an `extern` statement. If `getFunction` returns null then no previous version exists, so we’ll codegen one from the `Prototype`. In either case, we want to assert that the function is empty (i.e. has no body yet) before we start.

```c++
// Create a new basic block to start insertion into.
llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(getContext(), "entry", TheFunction);
getBuilder().SetInsertPoint(BB);

// Record the function arguments in the NamedValues map.
getNamedValues().clear();
for (auto &Arg : TheFunction->args())
  getNamedValues()[std::string(Arg.getName())] = &Arg;
```

Now we get to the point where the `Builder` is set up. The first line creates a new basic block (named “entry”), which is inserted into `TheFunction`. The second line then tells the builder that new instructions should be inserted into the end of the new basic block. Basic blocks in LLVM are an important part of functions that define the Control Flow Graph. Since we don’t have any control flow, our functions will only contain one block at this point. We’ll fix this in Chapter 5 :).

Next we add the function arguments to the `NamedValues` map (after first clearing it out) so that they’re accessible to `VariableExprAST` nodes.

```c++
if (llvm::Value *RetVal = Body->codegen()) {
  // Finish off the function.
  getBuilder().CreateRet(RetVal);

  // Validate the generated code, checking for consistency.
  verifyFunction(*TheFunction);

  return TheFunction;
}
```

Once the insertion point has been set up and the `NamedValues` map populated, we call the `codegen()` method for the root expression of the function. If no error happens, this emits code to compute the expression into the entry block and returns the value that was computed. Assuming no error, we then create an LLVM ret instruction, which completes the function. Once the function is built, we call verifyFunction, which is provided by LLVM. This function does a variety of consistency checks on the generated code, to determine if our compiler is doing everything right. Using this is important: it can catch a lot of bugs. Once the function is finished and validated, we return it.

```c++
  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}
```

The only piece left here is handling of the error case. For simplicity, we handle this by merely deleting the function we produced with the `eraseFromParent` method. This allows the user to redefine a function that they incorrectly typed in before: if we didn’t delete it, it would live in the symbol table, with a body, preventing future redefinition.

This code does have a bug, though: If the `FunctionAST::codegen()` method finds an existing IR Function, it does not validate its signature against the definition’s own prototype. This means that an earlier ‘extern’ declaration will take precedence over the function definition’s signature, which can cause codegen to fail, for instance if the function arguments are named differently. There are a number of ways to fix this bug, see what you can come up with! Here is a testcase:

```
extern foo(a);     # ok, defines foo.
def foo(b) b;      # Error: Unknown variable name. (decl using 'a' takes precedence).
```

#### 3.5. Driver Changes and Closing Thoughts

For now, code generation to LLVM doesn’t really get us much, except that we can look at the pretty IR calls. The sample code inserts calls to codegen into the `HandleDefinition`, `HandleExtern` etc functions, and then dumps out the LLVM IR. This gives a nice way to look at the LLVM IR for simple functions. For example:

```
ready> 4+5;
Read top-level expression:
define double @0() {
entry:
  ret double 9.000000e+00
}
```

Note how the parser turns the top-level expression into anonymous functions for us. This will be handy when we add JIT support in the next chapter. Also note that the code is very literally transcribed, no optimizations are being performed except simple constant folding done by `IRBuilder`. We will add optimizations explicitly in the next chapter.

```
ready> def foo(a b) a*a + 2*a*b + b*b;
Read function definition:
define double @foo(double %a, double %b) {
entry:
  %multmp = fmul double %a, %a
  %multmp1 = fmul double 2.000000e+00, %a
  %multmp2 = fmul double %multmp1, %b
  %addtmp = fadd double %multmp, %multmp2
  %multmp3 = fmul double %b, %b
  %addtmp4 = fadd double %addtmp, %multmp3
  ret double %addtmp4
}
```

This shows some simple arithmetic. Notice the striking similarity to the LLVM builder calls that we use to create the instructions.

```
ready> def bar(a) foo(a, 4.0) + bar(31337);
Read function definition:
define double @bar(double %a) {
entry:
  %calltmp = call double @foo(double %a, double 4.000000e+00)
  %calltmp1 = call double @bar(double 3.133700e+04)
  %addtmp = fadd double %calltmp, %calltmp1
  ret double %addtmp
}
```

This shows some function calls. Note that this function will take a long time to execute if you call it. In the future we’ll add conditional control flow to actually make recursion useful :).

```
ready> extern cos(x);
Read extern:
declare double @cos(double)

ready> cos(1.234);
Read top-level expression:
define double @1() {
entry:
  %calltmp = call double @cos(double 1.234000e+00)
  ret double %calltmp
}
```

This shows an extern for the libm `cos` function, and a call to it.

```
ready> ^D
; ModuleID = 'my cool jit'

define double @0() {
entry:
  %addtmp = fadd double 4.000000e+00, 5.000000e+00
  ret double %addtmp
}

define double @foo(double %a, double %b) {
entry:
  %multmp = fmul double %a, %a
  %multmp1 = fmul double 2.000000e+00, %a
  %multmp2 = fmul double %multmp1, %b
  %addtmp = fadd double %multmp, %multmp2
  %multmp3 = fmul double %b, %b
  %addtmp4 = fadd double %addtmp, %multmp3
  ret double %addtmp4
}

define double @bar(double %a) {
entry:
  %calltmp = call double @foo(double %a, double 4.000000e+00)
  %calltmp1 = call double @bar(double 3.133700e+04)
  %addtmp = fadd double %calltmp, %calltmp1
  ret double %addtmp
}

declare double @cos(double)

define double @1() {
entry:
  %calltmp = call double @cos(double 1.234000e+00)
  ret double %calltmp
}
```

When you quit the current demo (by sending an EOF via CTRL+D on Linux or CTRL+Z and ENTER on Windows), it dumps out the IR for the entire module generated. Here you can see the big picture with all the functions referencing each other.

This wraps up the third chapter of the Kaleidoscope tutorial. 


### Chapter 5: Extending the Language - Control Flow
=============================

*We extend the language by adding control flow constructs such as 'if' statements and 'for' loops. This involves discussing Static Single Assignment (SSA) form and control flow in LLVM.*

#### 5.1. Chapter 5 Introduction

Welcome to Chapter 5 of the “Implementing a language with LLVM” tutorial. Parts 1-4 described the implementation of the simple Kaleidoscope language and included support for generating LLVM IR, followed by optimizations and a JIT compiler. Unfortunately, as presented, Kaleidoscope is mostly useless: it has no control flow other than call and return. This means that you can’t have conditional branches in the code, significantly limiting its power. In this episode of “build that compiler”, we’ll extend Kaleidoscope to have an if/then/else expression plus a simple ‘for’ loop.

#### 5.2. If/Then/Else

Extending Kaleidoscope to support if/then/else is quite straightforward. It basically requires adding support for this “new” concept to the lexer, parser, AST, and LLVM code emitter. This example is nice, because it shows how easy it is to “grow” a language over time, incrementally extending it as new ideas are discovered.

Before we get going on “how” we add this extension, let’s talk about “what” we want. The basic idea is that we want to be able to write this sort of thing:

```
def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2);
```

In Kaleidoscope, every construct is an expression: there are no statements. As such, the if/then/else expression needs to return a value like any other. Since we’re using a mostly functional form, we’ll have it evaluate its conditional, then return the ‘then’ or ‘else’ value based on how the condition was resolved. This is very similar to the C `?:` expression.

The semantics of the if/then/else expression is that it evaluates the condition to a boolean equality value: 0.0 is considered to be false and everything else is considered to be true. If the condition is true, the first subexpression is evaluated and returned, if the condition is false, the second subexpression is evaluated and returned. Since Kaleidoscope allows side-effects, this behavior is important to nail down.

Now that we know what we “want”, let’s break this down into its constituent pieces.

##### 5.2.1. Lexer Extensions for If/Then/Else

The lexer extensions are straightforward. First we add new enum values for the relevant tokens:

```c++
// enum Token {
// control
    If = -6,
    Then = -7,
    Else = -8,
```

Once we have that, we recognize the new keywords in the lexer. This is pretty simple stuff:

```c++
...
    if (IdentifierStr == "def")
      return Def;
    if (IdentifierStr == "extern")
      return Extern;
    if (IdentifierStr == "if")
      return If;
    if (IdentifierStr == "then")
      return Then;
    if (IdentifierStr == "else")
      return Else;
    return Identifier;
```

##### 5.2.2. AST Extensions for If/Then/Else

To represent the new expression we add a new AST node for it:

```c++
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  llvm::Value *codegen() override;
};
```
The AST node just has pointers to the various subexpressions.

##### 5.2.3. Parser Extensions for If/Then/Else

Now that we have the relevant tokens coming from the lexer and we have the AST node to build, our parsing logic is relatively straightforward. First we define a new parsing function:

```c++
/// ifexpr ::= 'if' expression 'then' expression 'else' expression
std::unique_ptr<ExprAST> Tokenizer::ParseIfExpr() {
  GetNextToken(); // eat the if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;

  if (CurTok != Then)
    return LogError("expected then");
  GetNextToken(); // eat the then

  auto Then = ParseExpression();
  if (!Then)
    return nullptr;

  if (CurTok != Else)
    return LogError("expected else");

  GetNextToken();

  auto Else = ParseExpression();
  if (!Else)
    return nullptr;

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                     std::move(Else));
}
```
Next we hook it up as a primary expression:

```c++
std::unique_ptr<ExprAST> Tokenizer::ParsePrimary() {
  switch (CurTok) {
  case Identifier:
    return ParseIdentifierExpr();
  case Number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case If:
    return ParseIfExpr();

  default:
    return LogError("unknown token when expecting an expression");
  }
}
```

##### 5.2.4. LLVM IR for If/Then/Else

Now that we have it parsing and building the AST, the final piece is adding LLVM code generation support. This is the most interesting part of the if/then/else example, because this is where it starts to introduce new concepts. All of the code above has been thoroughly described in previous chapters.

To motivate the code we want to produce, let’s take a look at a simple example. Consider:

```
extern foo();
extern bar();
def baz(x) if x then foo() else bar();
```

If you disable optimizations, the code you’ll (soon) get from Kaleidoscope looks like this:

```
declare double @foo()

declare double @bar()

define double @baz(double %x) {
entry:
  %ifcond = fcmp one double %x, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:       ; preds = %entry
  %calltmp = call double @foo()
  br label %ifcont

else:       ; preds = %entry
  %calltmp1 = call double @bar()
  br label %ifcont

ifcont:     ; preds = %else, %then
  %iftmp = phi double [ %calltmp, %then ], [ %calltmp1, %else ]
  ret double %iftmp
}
```

To visualize the control flow graph, you can use a nifty feature of the LLVM ‘opt’ tool. If you put this LLVM IR into “t.ll” and run `llvm-as < t.ll | opt -passes=view-cfg`, a window will pop up and you’ll see the graph. (Will add the actual image from the tutorial later).

Another way to get this is to call `F->viewCFG()` or `F->viewCFGOnly()` (where F is a `Function*`) either by inserting actual calls into the code and recompiling or by calling these in the debugger. LLVM has many nice features for visualizing various graphs.

Getting back to the generated code, it is fairly simple: the `entry` block evaluates the conditional expression (“x” in our case here) and compares the result to 0.0 with the `fcmp one` instruction (‘one’ is “Ordered and Not Equal”). Based on the result of this expression, the code jumps to either the `then` or `else` blocks, which contain the expressions for the true/false cases.

Once the then/else blocks are finished executing, they both branch back to the `ifcont` block to execute the code that happens after the if/then/else. In this case the only thing left to do is to return to the caller of the function. The question then becomes: how does the code know which expression to return?

The answer to this question involves an important SSA operation: the `Phi` operation. If you’re not familiar with SSA, the wikipedia article is a good introduction and there are various other introductions to it available on your favorite search engine. The short version is that “execution” of the `Phi` operation requires “remembering” which block control came from. The `Phi` operation takes on the value corresponding to the input control block. In this case, if control comes in from the “then” block, it gets the value of `calltmp`. If control comes from the `else` block, it gets the value of `calltmp1`.

At this point, you are probably starting to think “Oh no! This means my simple and elegant front-end will have to start generating SSA form in order to use LLVM!”. Fortunately, this is not the case, and we strongly advise not implementing an SSA construction algorithm in your front-end unless there is an amazingly good reason to do so. In practice, there are two sorts of values that float around in code written for your average imperative programming language that might need `Phi` nodes:
 - Code that involves user variables: `x = 1; x = x + 1;`
 - Values that are implicit in the structure of your AST, such as the `Phi` node in this case.

In Chapter 7 of this tutorial (“mutable variables”), we’ll talk about #1 in depth. For now, just believe me that you don’t need SSA construction to handle this case. For #2, you have the choice of using the techniques that we will describe for #1, or you can insert Phi nodes directly, if convenient. In this case, it is really easy to generate the `Phi` node, so we choose to do it directly.

Okay, enough of the motivation and overview, let’s generate code!

##### 5.2.5. Code Generation for If/Then/Else

In order to generate code for this, we implement the codegen method for `IfExprAST`:

```c++
llvm::Value *IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  CondV = getBuilder().CreateFCmpONE(
      CondV, llvm::ConstantFP::get(getContext(), llvm::APFloat(0.0)), "ifcond");
```

This code is straightforward and similar to what we saw before. We emit the expression for the condition, then compare that value to zero to get a truth value as a 1-bit (bool) value.

```c++
llvm::Function *TheFunction = getBuilder().GetInsertBlock()->getParent(); // in the actual code, the variable name is slightly different, but it is not important

// Create blocks for the then and else cases.  Insert the 'then' block at the
// end of the function.
llvm::BasicBlock *ThenBB =
    llvm::BasicBlock::Create(getContext(), "then", TheFunction);
llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(getContext(), "else");
llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(getContext(), "ifcont");

getBuilder().CreateCondBr(CondV, ThenBB, ElseBB);
```
This code creates the basic blocks that are related to the if/then/else statement, and correspond directly to the blocks in the example above. The first line gets the current Function object that is being built. It gets this by asking the builder for the current BasicBlock, and asking that block for its “parent” (the function it is currently embedded into).

Once it has that, it creates three blocks. Note that it passes “TheFunction” into the constructor for the “then” block. This causes the constructor to automatically insert the new block into the end of the specified function. The other two blocks are created, but aren’t yet inserted into the function.

Once the blocks are created, we can emit the conditional branch that chooses between them. Note that creating new blocks does not implicitly affect the IRBuilder, so it is still inserting into the block that the condition went into. Also note that it is creating a branch to the “then” block and the “else” block, even though the “else” block isn’t inserted into the function yet. This is all ok: it is the standard way that LLVM supports forward references.

```c++
// Emit then value.
getBuilder().SetInsertPoint(ThenBB);

llvm::Value *ThenV = Then->codegen();
if (!ThenV)
  return nullptr;

getBuilder().CreateBr(MergeBB);
// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
ThenBB = getBuilder().GetInsertBlock();
```

After the conditional branch is inserted, we move the builder to start inserting into the “then” block. Strictly speaking, this call moves the insertion point to be at the end of the specified block. However, since the “then” block is empty, it also starts out by inserting at the beginning of the block. :)

Once the insertion point is set, we recursively codegen the “then” expression from the AST. To finish off the “then” block, we create an unconditional branch to the merge block. One interesting (and very important) aspect of the LLVM IR is that it requires all basic blocks to be “terminated” with a control flow instruction such as return or branch. This means that all control flow, including fall throughs must be made explicit in the LLVM IR. If you violate this rule, the verifier will emit an error.

The final line here is quite subtle, but is very important. The basic issue is that when we create the Phi node in the merge block, we need to set up the block/value pairs that indicate how the Phi will work. Importantly, the Phi node expects to have an entry for each predecessor of the block in the CFG. Why then, are we getting the current block when we just set it to ThenBB 5 lines above? The problem is that the “Then” expression may actually itself change the block that the Builder is emitting into if, for example, it contains a nested “if/then/else” expression. Because calling codegen() recursively could arbitrarily change the notion of the current block, we are required to get an up-to-date value for code that will set up the Phi node.

```c++
// Emit else block.
TheFunction->insert(TheFunction->end(), ElseBB);
getBuilder().SetInsertPoint(ElseBB);

llvm::Value *ElseV = Else->codegen();
if (!ElseV)
  return nullptr;

getBuilder().CreateBr(MergeBB);
// codegen of 'Else' can change the current block, update ElseBB for the PHI.
ElseBB = getBuilder().GetInsertBlock();
```

Code generation for the `else` block is basically identical to codegen for the ‘then’ block. The only significant difference is the first line, which adds the `else` block to the function. Recall previously that the `else` block was created, but not added to the function. Now that the `then` and `else` blocks are emitted, we can finish up with the merge code:

```c++
  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);
  getBuilder().SetInsertPoint(MergeBB);
  llvm::PHINode *PN =
      getBuilder().CreatePHI(llvm::Type::getDoubleTy(getContext()), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}
```

The first two lines here are now familiar: the first adds the `merge` block to the `Function` object (it was previously floating, like the else block above). The second changes the insertion point so that newly created code will go into the “merge” block. Once that is done, we need to create the `PHI` node and set up the block/value pairs for the `PHI`.

Finally, the `codegen()` function returns the phi node as the value computed by the if/then/else expression. In our example above, this returned value will feed into the code for the top-level function, which will create the return instruction.

Overall, we now have the ability to execute conditional code in Kaleidoscope. With this extension, Kaleidoscope is a fairly complete language that can calculate a wide variety of numeric functions. Next up we’ll add another useful expression that is familiar from non-functional languages…

#### 5.3. ‘for’ Loop Expression

Now that we know how to add basic control flow constructs to the language, we have the tools to add more powerful things. Let’s add something more aggressive, a ‘for’ expression:

```
extern putchard(char);
def printstar(n)
  for i = 1, i < n, 1.0 in
    putchard(42);  # ascii 42 = '*'

# print 100 '*' characters
printstar(100);
```

This expression defines a new variable (`i` in this case) which iterates from a starting value, while the condition (`i < n` in this case) is true, incrementing by an optional step value (`1.0` in this case). If the step value is omitted, it defaults to 1.0. While the loop is true, it executes its body expression. Because we don’t have anything better to return, we’ll just define the loop as always returning 0.0. In the future when we have mutable variables, it will get more useful.

As before, let’s talk about the changes that we need to Kaleidoscope to support this.

##### 5.3.1. Lexer Extensions for the ‘for’ Loop

The lexer extensions are the same sort of thing as for if/then/else:

```c++
// ... in enum Token ...
// control
    For = -9,
    In = -10,

// ... in gettok ...
    if (IdentifierStr == "for")
      return For;
    if (IdentifierStr == "in")
      return In;
```

##### 5.3.2. AST Extensions for the ‘for’ Loop

The AST node is just as simple. It basically boils down to capturing the variable name and the constituent expressions in the node.

```c++
/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST {
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
  ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
      : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}

  llvm::Value *codegen() override;
};
```

##### 5.3.3. Parser Extensions for the ‘for’ Loop

The parser code is also fairly standard. The only interesting thing here is handling of the optional step value. The parser code handles it by checking to see if the second comma is present. If not, it sets the step value to null in the AST node:

```c++
/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
std::unique_ptr<ExprAST> Tokenizer::ParseForExpr() {
  GetNextToken(); // eat the for.

  if (CurTok != Identifier)
    return LogError("expected identifier after for");

  std::string IdName = IdentifierStr;
  GetNextToken(); // eat identifier.

  if (CurTok != '=')
    return LogError("expected '=' after for");
  GetNextToken(); // eat '='.

  auto Start = ParseExpression();
  if (!Start)
    return nullptr;
  if (CurTok != ',')
    return LogError("expected ',' after for start value");
  GetNextToken();

  auto End = ParseExpression();
  if (!End)
    return nullptr;

  // The step value is optional.
  std::unique_ptr<ExprAST> Step;
  if (CurTok == ',') {
    GetNextToken();
    Step = ParseExpression();
    if (!Step)
      return nullptr;
  }

  if (CurTok != In)
    return LogError("expected 'in' after for");
  GetNextToken(); // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                      std::move(Step), std::move(Body));
}
```

And again we hook it up as a primary expression:

```c++
std::unique_ptr<ExprAST> Tokenizer::ParsePrimary() {
  switch (CurTok) {
  case Identifier:
    return ParseIdentifierExpr();
  case Number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case If:
    return ParseIfExpr();
  case For:
    return ParseForExpr();

  default:
    return LogError("unknown token when expecting an expression");
  }
}
```

##### 5.3.4. LLVM IR for the ‘for’ Loop

Now we get to the good part: the LLVM IR we want to generate for this thing. With the simple example above, we get this LLVM IR (note that this dump is generated with optimizations disabled for clarity):

```
declare double @putchard(double)

define double @printstar(double %n) {
entry:
  ; initial value = 1.0 (inlined into phi)
  br label %loop

loop:       ; preds = %loop, %entry
  %i = phi double [ 1.000000e+00, %entry ], [ %nextvar, %loop ]
  ; body
  %calltmp = call double @putchard(double 4.200000e+01)
  ; increment
  %nextvar = fadd double %i, 1.000000e+00

  ; termination test
  %cmptmp = fcmp ult double %i, %n
  %booltmp = uitofp i1 %cmptmp to double
  %loopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:      ; preds = %loop
  ; loop always returns 0.0
  ret double 0.000000e+00
}
```

This loop contains all the same constructs we saw before: a phi node, several expressions, and some basic blocks. Let’s see how this fits together.

##### 5.3.5. Code Generation for the ‘for’ Loop

The first part of codegen is very simple: we just output the start expression for the loop value:

```c++
llvm::Value *ForExprAST::codegen() {
  // Emit the start code first, without 'variable' in scope.
  llvm::Value *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;
```

With this out of the way, the next step is to set up the LLVM basic block for the start of the loop body. In the case above, the whole loop body is one block, but remember that the body code itself could consist of multiple blocks (e.g. if it contains an if/then/else or a for/in expression).

```c++
  // Make the new basic block for the loop header, inserting after current
  // block.
  llvm::Function *TheFunction = getBuilder().GetInsertBlock()->getParent();
  llvm::BasicBlock *PreheaderBB = getBuilder().GetInsertBlock();
  llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(getContext(), "loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  getBuilder().CreateBr(LoopBB);
```

This code is similar to what we saw for if/then/else. Because we will need it to create the `Phi` node, we remember the block that falls through into the loop. Once we have that, we create the actual block that starts the loop and create an unconditional branch for the fall-through between the two blocks.

```c++
  // Start insertion in LoopBB.
  getBuilder().SetInsertPoint(LoopBB);

  // Start the PHI node with an entry for Start.
  llvm::PHINode *Variable = getBuilder().CreatePHI(llvm::Type::getDoubleTy(getContext()),
                                       2, VarName);
  Variable->addIncoming(StartVal, PreheaderBB);
```

Now that the `preheader` for the loop is set up, we switch to emitting code for the loop body. To begin with, we move the insertion point and create the `PHI` node for the loop induction variable. Since we already know the incoming value for the starting value, we add it to the `Phi` node. Note that the `Phi` will eventually get a second value for the backedge, but we can’t set it up yet (because it doesn’t exist!).

```c++
  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  llvm::Value *OldVal = getNamedValues()[VarName];
  getNamedValues()[VarName] = Variable;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen())
    return nullptr;
```

Now the code starts to get more interesting. Our ‘for’ loop introduces a new variable to the symbol table. This means that our symbol table can now contain either function arguments or loop variables. To handle this, before we codegen the body of the loop, we add the loop variable as the current value for its name. Note that it is possible that there is a variable of the same name in the outer scope. It would be easy to make this an error (emit an error and return null if there is already an entry for `VarName`) but we choose to allow shadowing of variables. In order to handle this correctly, we remember the `Value` that we are potentially shadowing in `OldVal` (which will be null if there is no shadowed variable).

Once the loop variable is set into the symbol table, the code recursively codegen’s the body. This allows the body to use the loop variable: any references to it will naturally find it in the symbol table.

```c++
  // Emit the step value.
  llvm::Value *StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen();
    if (!StepVal)
      return nullptr;
  } else {
    // If not specified, use 1.0.
    StepVal = llvm::ConstantFP::get(getContext(), llvm::APFloat(1.0));
  }

  llvm::Value *NextVar = getBuilder().CreateFAdd(Variable, StepVal, "nextvar");
```

Now that the body is emitted, we compute the next value of the iteration variable by adding the step value, or 1.0 if it isn’t present. `NextVar` will be the value of the loop variable on the next iteration of the loop.

```c++
  // Compute the end condition.
  llvm::Value *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = getBuilder().CreateFCmpONE(
      EndCond, llvm::ConstantFP::get(getContext(), llvm::APFloat(0.0)), "loopcond");
```

Finally, we evaluate the exit value of the loop, to determine whether the loop should exit. This mirrors the condition evaluation for the if/then/else statement.

```c++
  // Create the "after loop" block and insert it.
  llvm::BasicBlock *LoopEndBB = getBuilder().GetInsertBlock();
  llvm::BasicBlock *AfterBB =
      llvm::BasicBlock::Create(getContext(), "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  getBuilder().CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  getBuilder().SetInsertPoint(AfterBB);
```

With the code for the body of the loop complete, we just need to finish up the control flow for it. This code remembers the end block (for the phi node), then creates the block for the loop exit (`afterloop`). Based on the value of the exit condition, it creates a conditional branch that chooses between executing the loop again and exiting the loop. Any future code is emitted in the “afterloop” block, so it sets the insertion position to it.

```c++
  // Add a new entry to the PHI node for the backedge.
  Variable->addIncoming(NextVar, LoopEndBB);

  // Restore the unshadowed variable.
  if (OldVal)
    getNamedValues()[VarName] = OldVal;
  else
    NamedValues.erase(VarName);

  // for expr always returns 0.0.
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(getContext()));
}
```

The final code handles various cleanups: now that we have the `NextVar` value, we can add the incoming value to the loop `PHI` node. After that, we remove the loop variable from the symbol table, so that it isn’t in scope after the for loop. Finally, code generation of the for loop always returns 0.0, so that is what we return from `ForExprAST::codegen()`.

With this, we conclude the “adding control flow to Kaleidoscope” chapter of the tutorial. In this chapter we added two control flow constructs, and used them to motivate a couple of aspects of the LLVM IR that are important for front-end implementors to know.

### Chapter 7: Extending the Language - Mutable Variables
=============================

*This chapter introduces user-defined local variables and the assignment operator. We explore how LLVM handles variables without requiring explicit SSA form construction in the front-end.*


#### 7.1. Chapter 7 Introduction

Welcome to Chapter 7 of the “Implementing a language with LLVM” tutorial. In chapters 1 through 6, we’ve built a very respectable, albeit simple, functional programming language. In our journey, we learned some parsing techniques, how to build and represent an AST, how to build LLVM IR, and how to optimize the resultant code as well as JIT compile it.

While Kaleidoscope is interesting as a functional language, the fact that it is functional makes it “too easy” to generate LLVM IR for it. In particular, a functional language makes it very easy to build LLVM IR directly in SSA form. Since LLVM requires that the input code be in SSA form, this is a very nice property and it is often unclear to newcomers how to generate code for an imperative language with mutable variables.

The short (and happy) summary of this chapter is that there is no need for your front-end to build SSA form: LLVM provides highly tuned and well tested support for this, though the way it works is a bit unexpected for some.

#### 7.2. Why is this a hard problem?

To understand why mutable variables cause complexities in SSA construction, consider this extremely simple C example:

int G, H;
int test(_Bool Condition) {
  int X;
  if (Condition)
    X = G;
  else
    X = H;
  return X;
}

In this case, we have the variable “X”, whose value depends on the path executed in the program. Because there are two different possible values for X before the return instruction, a PHI node is inserted to merge the two values. The LLVM IR that we want for this example looks like this:

@G = weak global i32 0   ; type of @G is i32*
@H = weak global i32 0   ; type of @H is i32*

define i32 @test(i1 %Condition) {
entry:
  br i1 %Condition, label %cond_true, label %cond_false

cond_true:
  %X.0 = load i32, i32* @G
  br label %cond_next

cond_false:
  %X.1 = load i32, i32* @H
  br label %cond_next

cond_next:
  %X.2 = phi i32 [ %X.1, %cond_false ], [ %X.0, %cond_true ]
  ret i32 %X.2
}

In this example, the loads from the G and H global variables are explicit in the LLVM IR, and they live in the then/else branches of the if statement (cond_true/cond_false). In order to merge the incoming values, the X.2 phi node in the cond_next block selects the right value to use based on where control flow is coming from: if control flow comes from the cond_false block, X.2 gets the value of X.1. Alternatively, if control flow comes from cond_true, it gets the value of X.0. The intent of this chapter is not to explain the details of SSA form. For more information, see one of the many online references.

The question for this article is “who places the phi nodes when lowering assignments to mutable variables?”. The issue here is that LLVM requires that its IR be in SSA form: there is no “non-ssa” mode for it. However, SSA construction requires non-trivial algorithms and data structures, so it is inconvenient and wasteful for every front-end to have to reproduce this logic.

#### 7.3. Memory in LLVM

The ‘trick’ here is that while LLVM does require all register values to be in SSA form, it does not require (or permit) memory objects to be in SSA form. In the example above, note that the loads from G and H are direct accesses to G and H: they are not renamed or versioned. This differs from some other compiler systems, which do try to version memory objects. In LLVM, instead of encoding dataflow analysis of memory into the LLVM IR, it is handled with Analysis Passes which are computed on demand.

With this in mind, the high-level idea is that we want to make a stack variable (which lives in memory, because it is on the stack) for each mutable object in a function. To take advantage of this trick, we need to talk about how LLVM represents stack variables.

In LLVM, all memory accesses are explicit with load/store instructions, and it is carefully designed not to have (or need) an “address-of” operator. Notice how the type of the @G/@H global variables is actually “i32*” even though the variable is defined as “i32”. What this means is that @G defines space for an i32 in the global data area, but its name actually refers to the address for that space. Stack variables work the same way, except that instead of being declared with global variable definitions, they are declared with the LLVM alloca instruction:

```
define i32 @example() {
entry:
  %X = alloca i32           ; type of %X is i32*.
  ...
  %tmp = load i32, i32* %X  ; load the stack value %X from the stack.
  %tmp2 = add i32 %tmp, 1   ; increment it
  store i32 %tmp2, i32* %X  ; store it back
  ...
```

This code shows an example of how you can declare and manipulate a stack variable in the LLVM IR. Stack memory allocated with the alloca instruction is fully general: you can pass the address of the stack slot to functions, you can store it in other variables, etc. In our example above, we could rewrite the example to use the alloca technique to avoid using a PHI node:

```
@G = weak global i32 0   ; type of @G is i32*
@H = weak global i32 0   ; type of @H is i32*

define i32 @test(i1 %Condition) {
entry:
  %X = alloca i32           ; type of %X is i32*.
  br i1 %Condition, label %cond_true, label %cond_false

cond_true:
  %X.0 = load i32, i32* @G
  store i32 %X.0, i32* %X   ; Update X
  br label %cond_next

cond_false:
  %X.1 = load i32, i32* @H
  store i32 %X.1, i32* %X   ; Update X
  br label %cond_next

cond_next:
  %X.2 = load i32, i32* %X  ; Read X
  ret i32 %X.2
}
```

With this, we have discovered a way to handle arbitrary mutable variables without the need to create Phi nodes at all:

    Each mutable variable becomes a stack allocation.

    Each read of the variable becomes a load from the stack.

    Each update of the variable becomes a store to the stack.

    Taking the address of a variable just uses the stack address directly.

While this solution has solved our immediate problem, it introduced another one: we have now apparently introduced a lot of stack traffic for very simple and common operations, a major performance problem. Fortunately for us, the LLVM optimizer has a highly-tuned optimization pass named “mem2reg” that handles this case, promoting allocas like this into SSA registers, inserting Phi nodes as appropriate. If you run this example through the pass, for example, you’ll get:

```
$ llvm-as < example.ll | opt -passes=mem2reg | llvm-dis
@G = weak global i32 0
@H = weak global i32 0

define i32 @test(i1 %Condition) {
entry:
  br i1 %Condition, label %cond_true, label %cond_false

cond_true:
  %X.0 = load i32, i32* @G
  br label %cond_next

cond_false:
  %X.1 = load i32, i32* @H
  br label %cond_next

cond_next:
  %X.01 = phi i32 [ %X.1, %cond_false ], [ %X.0, %cond_true ]
  ret i32 %X.01
}
```

The mem2reg pass implements the standard “iterated dominance frontier” algorithm for constructing SSA form and has a number of optimizations that speed up (very common) degenerate cases. The mem2reg optimization pass is the answer to dealing with mutable variables, and we highly recommend that you depend on it. Note that mem2reg only works on variables in certain circumstances:

    mem2reg is alloca-driven: it looks for allocas and if it can handle them, it promotes them. It does not apply to global variables or heap allocations.

    mem2reg only looks for alloca instructions in the entry block of the function. Being in the entry block guarantees that the alloca is only executed once, which makes analysis simpler.

    mem2reg only promotes allocas whose uses are direct loads and stores. If the address of the stack object is passed to a function, or if any funny pointer arithmetic is involved, the alloca will not be promoted.

    mem2reg only works on allocas of first class values (such as pointers, scalars and vectors), and only if the array size of the allocation is 1 (or missing in the .ll file). mem2reg is not capable of promoting structs or arrays to registers. Note that the “sroa” pass is more powerful and can promote structs, “unions”, and arrays in many cases.

All of these properties are easy to satisfy for most imperative languages, and we’ll illustrate it below with Kaleidoscope. The final question you may be asking is: should I bother with this nonsense for my front-end? Wouldn’t it be better if I just did SSA construction directly, avoiding use of the mem2reg optimization pass? In short, we strongly recommend that you use this technique for building SSA form, unless there is an extremely good reason not to. Using this technique is:

    Proven and well tested: clang uses this technique for local mutable variables. As such, the most common clients of LLVM are using this to handle a bulk of their variables. You can be sure that bugs are found fast and fixed early.

    Extremely Fast: mem2reg has a number of special cases that make it fast in common cases as well as fully general. For example, it has fast-paths for variables that are only used in a single block, variables that only have one assignment point, good heuristics to avoid insertion of unneeded phi nodes, etc.

    Needed for debug info generation: Debug information in LLVM relies on having the address of the variable exposed so that debug info can be attached to it. This technique dovetails very naturally with this style of debug info.

If nothing else, this makes it much easier to get your front-end up and running, and is very simple to implement. Let’s extend Kaleidoscope with mutable variables now!

#### 7.4. Mutable Variables in Kaleidoscope

Now that we know the sort of problem we want to tackle, let’s see what this looks like in the context of our little Kaleidoscope language. We’re going to add two features:

    The ability to mutate variables with the ‘=’ operator.

    The ability to define new variables.

While the first item is really what this is about, we only have variables for incoming arguments as well as for induction variables, and redefining those only goes so far :). Also, the ability to define new variables is a useful thing regardless of whether you will be mutating them. Here’s a motivating example that shows how we could use these:
```
# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;

# Recursive fib, we could do this before.
def fib(x)
  if (x < 3) then
    1
  else
    fib(x-1)+fib(x-2);

# Iterative fib.
def fibi(x)
  var a = 1, b = 1, c in
  (for i = 3, i < x in
     c = a + b :
     a = b :
     b = c) :
  b;

# Call it.
fibi(10);
```

In order to mutate variables, we have to change our existing variables to use the “alloca trick”. Once we have that, we’ll add our new operator, then extend Kaleidoscope to support new variable definitions.

#### 7.5. Adjusting Existing Variables for Mutation

The symbol table in Kaleidoscope is managed at code generation time by the ‘NamedValues’ map. This map currently keeps track of the LLVM “Value*” that holds the double value for the named variable. In order to support mutation, we need to change this slightly, so that NamedValues holds the memory location of the variable in question. Note that this change is a refactoring: it changes the structure of the code, but does not (by itself) change the behavior of the compiler. All of these changes are isolated in the Kaleidoscope code generator.

At this point in Kaleidoscope’s development, it only supports variables for two things: incoming arguments to functions and the induction variable of ‘for’ loops. For consistency, we’ll allow mutation of these variables in addition to other user-defined variables. This means that these will both need memory locations.

To start our transformation of Kaleidoscope, we’ll change the NamedValues map so that it maps to AllocaInst* instead of Value*. Once we do this, the C++ compiler will tell us what parts of the code we need to update:

static std::map<std::string, AllocaInst*> NamedValues;

Also, since we will need to create these allocas, we’ll use a helper function that ensures that the allocas are created in the entry block of the function:

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          const std::string &VarName) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                 TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Type::getDoubleTy(getContext()), nullptr,
                           VarName);
}

This funny looking code creates an IRBuilder object that is pointing at the first instruction (.begin()) of the entry block. It then creates an alloca with the expected name and returns it. Because all values in Kaleidoscope are doubles, there is no need to pass in a type to use.

With this in place, the first functionality change we want to make belongs to variable references. In our new scheme, variables live on the stack, so code generating a reference to them actually needs to produce a load from the stack slot:

Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  AllocaInst *A = getNamedValues()[Name];
  if (!A)
    return LogErrorV("Unknown variable name");

  // Load the value.
  return getBuilder().CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

As you can see, this is pretty straightforward. Now we need to update the things that define the variables to set up the alloca. We’ll start with ForExprAST::codegen() (see the full code listing for the unabridged code):

Function *TheFunction = getBuilder().GetInsertBlock()->getParent();

// Create an alloca for the variable in the entry block.
AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

// Emit the start code first, without 'variable' in scope.
Value *StartVal = Start->codegen();
if (!StartVal)
  return nullptr;

// Store the value into the alloca.
getBuilder().CreateStore(StartVal, Alloca);
...

// Compute the end condition.
Value *EndCond = End->codegen();
if (!EndCond)
  return nullptr;

// Reload, increment, and restore the alloca.  This handles the case where
// the body of the loop mutates the variable.
Value *CurVar = getBuilder().CreateLoad(Alloca->getAllocatedType(), Alloca,
                                    VarName.c_str());
Value *NextVar = getBuilder().CreateFAdd(CurVar, StepVal, "nextvar");
getBuilder().CreateStore(NextVar, Alloca);
...

This code is virtually identical to the code before we allowed mutable variables. The big difference is that we no longer have to construct a PHI node, and we use load/store to access the variable as needed.

To support mutable argument variables, we need to also make allocas for them. The code for this is also pretty simple:

Function *FunctionAST::codegen() {
  ...
  getBuilder().SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable.
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());

    // Store the initial value into the alloca.
    getBuilder().CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    getNamedValues()[std::string(Arg.getName())] = Alloca;
  }

  if (Value *RetVal = Body->codegen()) {
    ...

For each argument, we make an alloca, store the input value to the function into the alloca, and register the alloca as the memory location for the argument. This method gets invoked by FunctionAST::codegen() right after it sets up the entry block for the function.

The final missing piece is adding the mem2reg pass, which allows us to get good codegen once again:

// Promote allocas to registers.
TheFPM->add(createPromoteMemoryToRegisterPass());
// Do simple "peephole" optimizations and bit-twiddling optzns.
TheFPM->add(createInstructionCombiningPass());
// Reassociate expressions.
TheFPM->add(createReassociatePass());
...

It is interesting to see what the code looks like before and after the mem2reg optimization runs. For example, this is the before/after code for our recursive fib function. Before the optimization:

define double @fib(double %x) {
entry:
  %x1 = alloca double
  store double %x, double* %x1
  %x2 = load double, double* %x1
  %cmptmp = fcmp ult double %x2, 3.000000e+00
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:       ; preds = %entry
  br label %ifcont

else:       ; preds = %entry
  %x3 = load double, double* %x1
  %subtmp = fsub double %x3, 1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %x4 = load double, double* %x1
  %subtmp5 = fsub double %x4, 2.000000e+00
  %calltmp6 = call double @fib(double %subtmp5)
  %addtmp = fadd double %calltmp, %calltmp6
  br label %ifcont

ifcont:     ; preds = %else, %then
  %iftmp = phi double [ 1.000000e+00, %then ], [ %addtmp, %else ]
  ret double %iftmp
}

Here there is only one variable (x, the input argument) but you can still see the extremely simple-minded code generation strategy we are using. In the entry block, an alloca is created, and the initial input value is stored into it. Each reference to the variable does a reload from the stack. Also, note that we didn’t modify the if/then/else expression, so it still inserts a PHI node. While we could make an alloca for it, it is actually easier to create a PHI node for it, so we still just make the PHI.

Here is the code after the mem2reg pass runs:

define double @fib(double %x) {
entry:
  %cmptmp = fcmp ult double %x, 3.000000e+00
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:
  br label %ifcont

else:
  %subtmp = fsub double %x, 1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp5 = fsub double %x, 2.000000e+00
  %calltmp6 = call double @fib(double %subtmp5)
  %addtmp = fadd double %calltmp, %calltmp6
  br label %ifcont

ifcont:     ; preds = %else, %then
  %iftmp = phi double [ 1.000000e+00, %then ], [ %addtmp, %else ]
  ret double %iftmp
}

This is a trivial case for mem2reg, since there are no redefinitions of the variable. The point of showing this is to calm your tension about inserting such blatant inefficiencies :).

After the rest of the optimizers run, we get:

define double @fib(double %x) {
entry:
  %cmptmp = fcmp ult double %x, 3.000000e+00
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp ueq double %booltmp, 0.000000e+00
  br i1 %ifcond, label %else, label %ifcont

else:
  %subtmp = fsub double %x, 1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp5 = fsub double %x, 2.000000e+00
  %calltmp6 = call double @fib(double %subtmp5)
  %addtmp = fadd double %calltmp, %calltmp6
  ret double %addtmp

ifcont:
  ret double 1.000000e+00
}

Here we see that the simplifycfg pass decided to clone the return instruction into the end of the ‘else’ block. This allowed it to eliminate some branches and the PHI node.

Now that all symbol table references are updated to use stack variables, we’ll add the assignment operator.

#### 7.6. New Assignment Operator

With our current framework, adding a new assignment operator is really simple. We will parse it just like any other binary operator, but handle it internally (instead of allowing the user to define it). The first step is to set a precedence:

int main() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['='] = 2;
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;

Now that the parser knows the precedence of the binary operator, it takes care of all the parsing and AST generation. We just need to implement codegen for the assignment operator. This looks like:

Value *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // This assume we're building without RTTI because LLVM builds that way by
    // default. If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    VariableExprAST *LHSE = static_cast<VariableExprAST*>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");

Unlike the rest of the binary operators, our assignment operator doesn’t follow the “emit LHS, emit RHS, do computation” model. As such, it is handled as a special case before the other binary operators are handled. The other strange thing is that it requires the LHS to be a variable. It is invalid to have “(x+1) = expr” - only things like “x = expr” are allowed.

  // Codegen the RHS.
  Value *Val = RHS->codegen();
  if (!Val)
    return nullptr;

  // Look up the name.
  Value *Variable = getNamedValues()[LHSE->getName()];
  if (!Variable)
    return LogErrorV("Unknown variable name");

  getBuilder().CreateStore(Val, Variable);
  return Val;
}
...

Once we have the variable, codegen’ing the assignment is straightforward: we emit the RHS of the assignment, create a store, and return the computed value. Returning a value allows for chained assignments like “X = (Y = Z)”.

Now that we have an assignment operator, we can mutate loop variables and arguments. For example, we can now run code like this:

```
# Function to print a double.
extern printd(x);

# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;

def test(x)
  printd(x) :
  x = 4 :
  printd(x);

test(123);
```

When run, this example prints “123” and then “4”, showing that we did actually mutate the value! Okay, we have now officially implemented our goal: getting this to work requires SSA construction in the general case. However, to be really useful, we want the ability to define our own local variables, let’s add this next!

#### 7.7. User-defined Local Variables

Adding var/in is just like any other extension we made to Kaleidoscope: we extend the lexer, the parser, the AST and the code generator. The first step for adding our new ‘var/in’ construct is to extend the lexer. As before, this is pretty trivial, the code looks like this:

enum Token {
  ...
  // var definition
  tok_var = -13
...
}
...
static int gettok() {
...
    if (IdentifierStr == "in")
      return tok_in;
    if (IdentifierStr == "binary")
      return tok_binary;
    if (IdentifierStr == "unary")
      return tok_unary;
    if (IdentifierStr == "var")
      return tok_var;
    return tok_identifier;
...

The next step is to define the AST node that we will construct. For var/in, it looks like this:

```
/// VarExprAST - Expression class for var/in
class VarExprAST : public ExprAST {
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::unique_ptr<ExprAST> Body;

public:
  VarExprAST(std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
             std::unique_ptr<ExprAST> Body)
    : VarNames(std::move(VarNames)), Body(std::move(Body)) {}

  Value *codegen() override;
};
```

var/in allows a list of names to be defined all at once, and each name can optionally have an initializer value. As such, we capture this information in the VarNames vector. Also, var/in has a body, this body is allowed to access the variables defined by the var/in.

With this in place, we can define the parser pieces. The first thing we do is add it as a primary expression:

```
/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
///   ::= varexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case tok_if:
    return ParseIfExpr();
  case tok_for:
    return ParseForExpr();
  case tok_var:
    return ParseVarExpr();
  }
}
```

Next we define ParseVarExpr:

```
/// varexpr ::= 'var' identifier ('=' expression)?
//                    (',' identifier ('=' expression)?)* 'in' expression
static std::unique_ptr<ExprAST> ParseVarExpr() {
  getNextToken();  // eat the var.

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

  // At least one variable name is required.
  if (CurTok != tok_identifier)
    return LogError("expected identifier after var");
```

The first part of this code parses the list of identifier/expr pairs into the local VarNames vector.

```
while (true) {
  std::string Name = IdentifierStr;
  getNextToken();  // eat identifier.

  // Read the optional initializer.
  std::unique_ptr<ExprAST> Init;
  if (CurTok == '=') {
    getNextToken(); // eat the '='.

    Init = ParseExpression();
    if (!Init) return nullptr;
  }

  VarNames.push_back(std::make_pair(Name, std::move(Init)));

  // End of var list, exit loop.
  if (CurTok != ',') break;
  getNextToken(); // eat the ','.

  if (CurTok != tok_identifier)
    return LogError("expected identifier list after var");
}
```

Once all the variables are parsed, we then parse the body and create the AST node:

```
  // At this point, we have to have 'in'.
  if (CurTok != tok_in)
    return LogError("expected 'in' keyword after 'var'");
  getNextToken();  // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<VarExprAST>(std::move(VarNames),
                                       std::move(Body));
}
```

Now that we can parse and represent the code, we need to support emission of LLVM IR for it. This code starts out with:

```
Value *VarExprAST::codegen() {
  std::vector<AllocaInst *> OldBindings;

  Function *TheFunction = getBuilder().GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    const std::string &VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second.get();
```


Basically it loops over all the variables, installing them one at a time. For each variable we put into the symbol table, we remember the previous value that we replace in OldBindings.

```
  // Emit the initializer before adding the variable to scope, this prevents
  // the initializer from referencing the variable itself, and permits stuff
  // like this:
  //  var a = 1 in
  //    var a = a in ...   # refers to outer 'a'.
  Value *InitVal;
  if (Init) {
    InitVal = Init->codegen();
    if (!InitVal)
      return nullptr;
  } else { // If not specified, use 0.0.
    InitVal = ConstantFP::get(getContext(), APFloat(0.0));
  }

  AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
  getBuilder().CreateStore(InitVal, Alloca);

  // Remember the old variable binding so that we can restore the binding when
  // we unrecurse.
  OldBindings.push_back(getNamedValues()[VarName]);

  // Remember this binding.
  getNamedValues()[VarName] = Alloca;
}
```

There are more comments here than code. The basic idea is that we emit the initializer, create the alloca, then update the symbol table to point to it. Once all the variables are installed in the symbol table, we evaluate the body of the var/in expression:

// Codegen the body, now that all vars are in scope.
Value *BodyVal = Body->codegen();
if (!BodyVal)
  return nullptr;

Finally, before returning, we restore the previous variable bindings:

  // Pop all our variables from scope.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
    getNamedValues()[VarNames[i].first] = OldBindings[i];

  // Return the body computation.
  return BodyVal;
}

The end result of all of this is that we get properly scoped variable definitions, and we even (trivially) allow mutation of them :).

With this, we completed what we set out to do. Our nice iterative fib example from the intro compiles and runs just fine. The mem2reg pass optimizes all of our stack variables into SSA registers, inserting PHI nodes where needed, and our front-end remains simple: no “iterated dominance frontier” computation anywhere in sight.


### Chapter 8: Compiling to Object Files


*We explain how to compile LLVM IR down to object files, similar to the process in a static compiler.*



#### 8.1. Chapter 8 Introduction

Welcome to Chapter 8 of the “Implementing a language with LLVM” tutorial. This chapter describes how to compile our language down to object files.
#### 8.2. Choosing a target

LLVM has native support for cross-compilation. You can compile to the architecture of your current machine, or just as easily compile for other architectures. In this tutorial, we’ll target the current machine.

To specify the architecture that you want to target, we use a string called a “target triple”. This takes the form <arch><sub>-<vendor>-<sys>-<abi> (see the cross compilation docs).

As an example, we can see what clang thinks is our current target triple:

```
$ clang --version | grep Target
Target: x86_64-unknown-linux-gnu
```

Running this command may show something different on your machine as you might be using a different architecture or operating system to me.

Fortunately, we don’t need to hard-code a target triple to target the current machine. LLVM provides sys::getDefaultTargetTriple, which returns the target triple of the current machine.

```
auto TargetTriple = sys::getDefaultTargetTriple();
```

LLVM doesn’t require us to link in all the target functionality. For example, if we’re just using the JIT, we don’t need the assembly printers. Similarly, if we’re only targeting certain architectures, we can only link in the functionality for those architectures.

For this example, we’ll initialize all the targets for emitting object code.

```
InitializeAllTargetInfos();
InitializeAllTargets();
InitializeAllTargetMCs();
InitializeAllAsmParsers();
InitializeAllAsmPrinters();

We can now use our target triple to get a Target:

std::string Error;
auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

// Print an error and exit if we couldn't find the requested target.
// This generally occurs if we've forgotten to initialise the
// TargetRegistry or we have a bogus target triple.
if (!Target) {
  errs() << Error;
  return 1;
}
```

#### 8.3. Target Machine

We will also need a TargetMachine. This class provides a complete machine description of the machine we’re targeting. If we want to target a specific feature (such as SSE) or a specific CPU (such as Intel’s Sandylake), we do so now.

To see which features and CPUs that LLVM knows about, we can use llc. For example, let’s look at x86:

```
$ llvm-as < /dev/null | llc -march=x86 -mattr=help
Available CPUs for this target:

  amdfam10      - Select the amdfam10 processor.
  athlon        - Select the athlon processor.
  athlon-4      - Select the athlon-4 processor.
  ...

Available features for this target:

  16bit-mode            - 16-bit mode (i8086).
  32bit-mode            - 32-bit mode (80386).
  3dnow                 - Enable 3DNow! instructions.
  3dnowa                - Enable 3DNow! Athlon instructions.
  ...
```

For our example, we’ll use the generic CPU without any additional feature or target option.

```
auto CPU = "generic";
auto Features = "";

TargetOptions opt;
auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, Reloc::PIC_);
```

#### 8.4. Configuring the Module

We’re now ready to configure our module, to specify the target and data layout. This isn’t strictly necessary, but the frontend performance guide recommends this. Optimizations benefit from knowing about the target and data layout.

TheModule->setDataLayout(TargetMachine->createDataLayout());
TheModule->setTargetTriple(TargetTriple);

#### 8.5. Emit Object Code

We’re ready to emit object code! Let’s define where we want to write our file to:

auto Filename = "output.o";
std::error_code EC;
raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

if (EC) {
  errs() << "Could not open file: " << EC.message();
  return 1;
}

Finally, we define a pass that emits object code, then we run that pass:

legacy::PassManager pass;
auto FileType = CodeGenFileType::ObjectFile;

if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
  errs() << "TargetMachine can't emit a file of this type";
  return 1;
}

pass.run(*TheModule);
dest.flush();

#### 8.6. Putting It All Together

Does it work? Let’s give it a try. We need to compile our code, but note that the arguments to llvm-config are different to the previous chapters.

$ clang++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -o toy

Let’s run it, and define a simple average function. Press Ctrl-D when you’re done.

$ ./toy
ready> def average(x y) (x + y) * 0.5;
^D
Wrote output.o

We have an object file! To test it, let’s write a simple program and link it with our output. Here’s the source code:

#include <iostream>

extern "C" {
    double average(double, double);
}

int main() {
    std::cout << "average of 3.0 and 4.0: " << average(3.0, 4.0) << std::endl;
}

We link our program to output.o and check the result is what we expected:

$ clang++ main.cpp output.o -o main
$ ./main
average of 3.0 and 4.0: 3.5


## Installation
TODO (but also is not that relevant)


## Contributing
Feel free to expand this with more chapters, more explanations, etc. However, I would only accept changes that either do not require changing code at all, or come with all the necessary code changes.

## Acknowledgments

This project is based on the ["My First Language Frontend with LLVM"](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) tutorial by the LLVM Project.
Changes Made:
- Skipped Chapters 4, 6, 9, and 10 for now.
- Adapted and modified the content to fit the scope of this project.
- Added new features and explanations in Chapters 5 and 7.

Original Source:
    LLVM Project: https://llvm.org/

Please note that this project is a derivative work based on the LLVM tutorial. All original content is credited to the LLVM Project and is used under the terms of the CC BY-SA 4.0 license.
