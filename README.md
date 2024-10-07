# Your Project Title

A comprehensive guide and implementation of [Your Project Description], based on the "My First Language Frontend with LLVM" tutorial by the LLVM Project.

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
- [Usage](#usage)
- [Contributing](#contributing)
- [License](#license)
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

Important note -- in the original tutorial, most the the llvm objects required globally for codegen, where made statis globals. Which is generally a _really bad idea_. Simply to avoid that here we will have the following base class (with a somewhat random name, feel free to rename it, as well as anything else):

```c++
class AST {
public:
  static llvm::LLVMContext &getContext() {
    static std::unique_ptr<llvm::LLVMContext> TheContext =
        std::make_unique<llvm::LLVMContext>();
    return *TheContext;
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
This will allow all classes below direct access to the objects needed. There is no interesting reason why should this be inherited either, it just was. Now, back to the tutorial.

The AST for a program captures its behavior in such a way that it is easy for later stages of the compiler (e.g. code generation) to interpret. We basically want one object for each construct in the language, and the AST should closely model the language. In Kaleidoscope, we have expressions, a prototype, and a function object. We’ll start with expressions first:

```c++
/// ExprAST - Base class for all expression nodes.
class ExprAST : public AST {
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
class PrototypeAST : public AST { // notice how here we inherite from just AST -- for access, not ExprAST
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
      : Name(Name), Args(std::move(Args)) {}

  const std::string &getName() const { return Name; }
  llvm::Function *codegen();
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST : public AST {
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

Similarly to the 2.1, the design for the tokenizer is different from the original. It is a singleton class with methods (instead of functions and a global). 

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

#### 3.1. Introduction
Welcome to Chapter 3 of the “Implementing a language with LLVM” tutorial. This chapter shows you how to transform the Abstract Syntax Tree, built in Chapter 2, into LLVM IR. This will teach you a little bit about how LLVM does things, as well as demonstrate how easy it is to use. It’s much more work to build a lexer and parser than it is to generate LLVM IR code. :)

Please note: the code in this chapter and later require LLVM 3.7 or later. LLVM 3.6 and before will not work with it. Also note that you need to use a version of this tutorial that matches your LLVM release: If you are using an official LLVM release, use the version of the documentation included with your release or on the llvm.org releases page.

#### 3.2. Code Generation Setup
In order to generate LLVM IR, we want some simple setup to get started. First we define virtual code generation (codegen) methods in each AST class:

```c++
/// ExprAST - Base class for all expression nodes.
class ExprAST {
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

The codegen() method says to emit IR for that AST node along with all the things it depends on, and they all return an LLVM Value object. “Value” is the class used to represent a “Static Single Assignment (SSA) register” or “SSA value” in LLVM. The most distinct aspect of SSA values is that their value is computed as the related instruction executes, and it does not get a new value until (and if) the instruction re-executes. In other words, there is no way to “change” an SSA value. For more information, please read up on Static Single Assignment - the concepts are really quite natural once you grok them.

Note that instead of adding virtual methods to the ExprAST class hierarchy, it could also make sense to use a visitor pattern or some other way to model this. Again, this tutorial won’t dwell on good software engineering practices: for our purposes, adding a virtual method is simplest.

The second thing we want is a “LogError” method like we used for the parser, which will be used to report errors found during code generation (for example, use of an undeclared parameter):

```c++
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<IRBuilder<>> Builder;
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;

Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}
```

The static variables will be used during code generation. TheContext is an opaque object that owns a lot of core LLVM data structures, such as the type and constant value tables. We don’t need to understand it in detail, we just need a single instance to pass into APIs that require it.

The Builder object is a helper object that makes it easy to generate LLVM instructions. Instances of the IRBuilder class template keep track of the current place to insert instructions and has methods to create new instructions.

TheModule is an LLVM construct that contains functions and global variables. In many ways, it is the top-level structure that the LLVM IR uses to contain code. It will own the memory for all of the IR that we generate, which is why the codegen() method returns a raw Value*, rather than a unique_ptr<Value>.

The NamedValues map keeps track of which values are defined in the current scope and what their LLVM representation is. (In other words, it is a symbol table for the code). In this form of Kaleidoscope, the only things that can be referenced are function parameters. As such, function parameters will be in this map when generating code for their function body.

With these basics in place, we can start talking about how to generate code for each expression. Note that this assumes that the Builder has been set up to generate code into something. For now, we’ll assume that this has already been done, and we’ll just use it to emit code.

#### 3.3. Expression Code Generation

Generating LLVM code for expression nodes is very straightforward: less than 45 lines of commented code for all four of our expression nodes. First we’ll do numeric literals:

```c++
Value *NumberExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}
```
In the LLVM IR, numeric constants are represented with the ConstantFP class, which holds the numeric value in an APFloat internally (APFloat has the capability of holding floating point constants of Arbitrary Precision). This code basically just creates and returns a ConstantFP. Note that in the LLVM IR that constants are all uniqued together and shared. For this reason, the API uses the “foo::get(…)” idiom instead of “new foo(..)” or “foo::Create(..)”.
```c++
Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  if (!V)
    LogErrorV("Unknown variable name");
  return V;
}
```

References to variables are also quite simple using LLVM. In the simple version of Kaleidoscope, we assume that the variable has already been emitted somewhere and its value is available. In practice, the only values that can be in the NamedValues map are function arguments. This code simply checks to see that the specified name is in the map (if not, an unknown variable is being referenced) and returns the value for it. In future chapters, we’ll add support for loop induction variables in the symbol table, and for local variables.
```c++
Value *BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "cmptmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext),
                                 "booltmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}
```
Binary operators start to get more interesting. The basic idea here is that we recursively emit code for the left-hand side of the expression, then the right-hand side, then we compute the result of the binary expression. In this code, we do a simple switch on the opcode to create the right LLVM instruction.

In the example above, the LLVM builder class is starting to show its value. IRBuilder knows where to insert the newly created instruction, all you have to do is specify what instruction to create (e.g. with CreateFAdd), which operands to use (L and R here) and optionally provide a name for the generated instruction.

One nice thing about LLVM is that the name is just a hint. For instance, if the code above emits multiple “addtmp” variables, LLVM will automatically provide each one with an increasing, unique numeric suffix. Local value names for instructions are purely optional, but it makes it much easier to read the IR dumps.

LLVM instructions are constrained by strict rules: for example, the Left and Right operands of an add instruction must have the same type, and the result type of the add must match the operand types. Because all values in Kaleidoscope are doubles, this makes for very simple code for add, sub and mul.

On the other hand, LLVM specifies that the fcmp instruction always returns an ‘i1’ value (a one bit integer). The problem with this is that Kaleidoscope wants the value to be a 0.0 or 1.0 value. In order to get these semantics, we combine the fcmp instruction with a uitofp instruction. This instruction converts its input integer into a floating point value by treating the input as an unsigned value. In contrast, if we used the sitofp instruction, the Kaleidoscope ‘<’ operator would return 0.0 and -1.0, depending on the input value.
```c++
Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = TheModule->getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}
```

Code generation for function calls is quite straightforward with LLVM. The code above initially does a function name lookup in the LLVM Module’s symbol table. Recall that the LLVM Module is the container that holds the functions we are JIT’ing. By giving each function the same name as what the user specifies, we can use the LLVM symbol table to resolve function names for us.

Once we have the function to call, we recursively codegen each argument that is to be passed in, and create an LLVM call instruction. Note that LLVM uses the native C calling conventions by default, allowing these calls to also call into standard library functions like “sin” and “cos”, with no additional effort.

This wraps up our handling of the four basic expressions that we have so far in Kaleidoscope. Feel free to go in and add some more. For example, by browsing the LLVM language reference you’ll find several other interesting instructions that are really easy to plug into our basic framework.

#### 3.4. Function Code Generation

Code generation for prototypes and functions must handle a number of details, which make their code less beautiful than expression code generation, but allows us to illustrate some important points. First, let’s talk about code generation for prototypes: they are used both for function bodies and external function declarations. The code starts with:

```c++
Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<Type*> Doubles(Args.size(),
                             Type::getDoubleTy(*TheContext));
  FunctionType *FT =
    FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

  Function *F =
    Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
```

This code packs a lot of power into a few lines. Note first that this function returns a “Function*” instead of a “Value*”. Because a “prototype” really talks about the external interface for a function (not the value computed by an expression), it makes sense for it to return the LLVM Function it corresponds to when codegen’d.

The call to FunctionType::get creates the FunctionType that should be used for a given Prototype. Since all function arguments in Kaleidoscope are of type double, the first line creates a vector of “N” LLVM double types. It then uses the Functiontype::get method to create a function type that takes “N” doubles as arguments, returns one double as a result, and that is not vararg (the false parameter indicates this). Note that Types in LLVM are uniqued just like Constants are, so you don’t “new” a type, you “get” it.

The final line above actually creates the IR Function corresponding to the Prototype. This indicates the type, linkage and name to use, as well as which module to insert into. “external linkage” means that the function may be defined outside the current module and/or that it is callable by functions outside the module. The Name passed in is the name the user specified: since “TheModule” is specified, this name is registered in “TheModule”s symbol table.

```c++
// Set names for all arguments.
unsigned Idx = 0;
for (auto &Arg : F->args())
  Arg.setName(Args[Idx++]);

return F;
```
Finally, we set the name of each of the function’s arguments according to the names given in the Prototype. This step isn’t strictly necessary, but keeping the names consistent makes the IR more readable, and allows subsequent code to refer directly to the arguments for their names, rather than having to look up them up in the Prototype AST.

At this point we have a function prototype with no body. This is how LLVM IR represents function declarations. For extern statements in Kaleidoscope, this is as far as we need to go. For function definitions however, we need to codegen and attach a function body.

```c++
Function *FunctionAST::codegen() {
    // First, check for an existing function from a previous 'extern' declaration.
  Function *TheFunction = TheModule->getFunction(Proto->getName());

  if (!TheFunction)
    TheFunction = Proto->codegen();

  if (!TheFunction)
    return nullptr;

  if (!TheFunction->empty())
    return (Function*)LogErrorV("Function cannot be redefined.");
```

For function definitions, we start by searching TheModule’s symbol table for an existing version of this function, in case one has already been created using an ‘extern’ statement. If Module::getFunction returns null then no previous version exists, so we’ll codegen one from the Prototype. In either case, we want to assert that the function is empty (i.e. has no body yet) before we start.

```c++
// Create a new basic block to start insertion into.
BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
Builder->SetInsertPoint(BB);

// Record the function arguments in the NamedValues map.
NamedValues.clear();
for (auto &Arg : TheFunction->args())
  NamedValues[std::string(Arg.getName())] = &Arg;
```

Now we get to the point where the Builder is set up. The first line creates a new basic block (named “entry”), which is inserted into TheFunction. The second line then tells the builder that new instructions should be inserted into the end of the new basic block. Basic blocks in LLVM are an important part of functions that define the Control Flow Graph. Since we don’t have any control flow, our functions will only contain one block at this point. We’ll fix this in Chapter 5 :).

Next we add the function arguments to the NamedValues map (after first clearing it out) so that they’re accessible to VariableExprAST nodes.

```c++
if (Value *RetVal = Body->codegen()) {
  // Finish off the function.
  Builder->CreateRet(RetVal);

  // Validate the generated code, checking for consistency.
  verifyFunction(*TheFunction);

  return TheFunction;
}
```

Once the insertion point has been set up and the NamedValues map populated, we call the codegen() method for the root expression of the function. If no error happens, this emits code to compute the expression into the entry block and returns the value that was computed. Assuming no error, we then create an LLVM ret instruction, which completes the function. Once the function is built, we call verifyFunction, which is provided by LLVM. This function does a variety of consistency checks on the generated code, to determine if our compiler is doing everything right. Using this is important: it can catch a lot of bugs. Once the function is finished and validated, we return it.

```c++
  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}
```

The only piece left here is handling of the error case. For simplicity, we handle this by merely deleting the function we produced with the eraseFromParent method. This allows the user to redefine a function that they incorrectly typed in before: if we didn’t delete it, it would live in the symbol table, with a body, preventing future redefinition.

This code does have a bug, though: If the FunctionAST::codegen() method finds an existing IR Function, it does not validate its signature against the definition’s own prototype. This means that an earlier ‘extern’ declaration will take precedence over the function definition’s signature, which can cause codegen to fail, for instance if the function arguments are named differently. There are a number of ways to fix this bug, see what you can come up with! Here is a testcase:

```
extern foo(a);     # ok, defines foo.
def foo(b) b;      # Error: Unknown variable name. (decl using 'a' takes precedence).
```

#### 3.5. Driver Changes and Closing Thoughts

For now, code generation to LLVM doesn’t really get us much, except that we can look at the pretty IR calls. The sample code inserts calls to codegen into the “HandleDefinition”, “HandleExtern” etc functions, and then dumps out the LLVM IR. This gives a nice way to look at the LLVM IR for simple functions. For example:

```
ready> 4+5;
Read top-level expression:
define double @0() {
entry:
  ret double 9.000000e+00
}
```

Note how the parser turns the top-level expression into anonymous functions for us. This will be handy when we add JIT support in the next chapter. Also note that the code is very literally transcribed, no optimizations are being performed except simple constant folding done by IRBuilder. We will add optimizations explicitly in the next chapter.
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

This shows an extern for the libm “cos” function, and a call to it.
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

This wraps up the third chapter of the Kaleidoscope tutorial. Up next, we’ll describe how to add JIT codegen and optimizer support to this so we can actually start running code!



*This chapter covers generating LLVM Intermediate Representation (IR) from the AST. We integrate LLVM into the project to facilitate code generation.*

### Chapter 5: Extending the Language - Control Flow


*We extend the language by adding control flow constructs such as 'if' statements and 'for' loops. This involves discussing Static Single Assignment (SSA) form and control flow in LLVM.*

### Chapter 7: Extending the Language - Mutable Variables


*This chapter introduces user-defined local variables and the assignment operator. We explore how LLVM handles variables without requiring explicit SSA form construction in the front-end.*

### Chapter 8: Compiling to Object Files


*We explain how to compile LLVM IR down to object files, similar to the process in a static compiler.*

## Installation
TODO (but also is not that relevant)

## Acknowledgments

This project is based on the ["My First Language Frontend with LLVM"](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) tutorial by the LLVM Project.
Changes Made:
- Skipped Chapters 4, 6, 9, and 10 for now.
- Adapted and modified the content to fit the scope of this project.
- Added new features and explanations in Chapters 5 and 7.

Original Source:
    LLVM Project: https://llvm.org/

Please note that this project is a derivative work based on the LLVM tutorial. All original content is credited to the LLVM Project and is used under the terms of the CC BY-SA 4.0 license.
