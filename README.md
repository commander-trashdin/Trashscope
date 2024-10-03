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

```
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

```
int Tokenizer::GetTok() {
  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();
```

It works by calling the C getchar() function to read characters one at a time from standard input. It eats them as it recognizes them and stores the last character read, but not processed, in LastChar. The first thing that it has to do is ignore whitespace between tokens. This is accomplished with the loop above.

The next thing GetTok needs to do is recognize identifiers and specific keywords like `def`. Kaleidoscope does this with this simple loop:

```
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

```
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

```
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

```
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

```
/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() {}
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;
public:
  NumberExprAST(double val) : Val(val) {}
};
```

The code above shows the definition of the base ExprAST class and one subclass which we use for numeric literals. The important thing to note about this code is that the NumberExprAST class captures the numeric value of the literal as an instance variable. This allows later phases of the compiler to know what the stored numeric value is.

Right now we only create the AST, so there are no useful accessor methods on them. It would be very easy to add a virtual method to pretty print the code, for example. Here are the other expression AST node definitions that we'll use in the basic form of the Kaleidoscope language:

```
/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;
public:
  VariableExprAST(const std::string &name) : Name(name) {}
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  ExprAST *LHS, *RHS;
public:
  BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
    : Op(op), LHS(lhs), RHS(rhs) {}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<ExprAST*> Args;
public:
  CallExprAST(const std::string &callee, std::vector<ExprAST*> &args)
    : Callee(callee), Args(args) {}
};
```

This is all (intentionally) rather straight-forward: variables capture the variable name, binary operators capture their opcode (e.g. `+`), and calls capture a function name as well as a list of any argument expressions. One thing that is nice about our AST is that it captures the language features without talking about the syntax of the language. Note that there is no discussion about precedence of binary operators, lexical structure, etc.

For our basic language, these are all of the expression nodes we'll define. Because it doesn't have conditional control flow, it isn't Turing-complete; we'll fix that in a later installment. The two things we need next are a way to talk about the interface to a function, and a way to talk about functions themselves:

```
/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
public:
  PrototypeAST(const std::string &name, const std::vector<std::string> &args)
    : Name(name), Args(args) {}
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  PrototypeAST *Proto;
  ExprAST *Body;
public:
  FunctionAST(PrototypeAST *proto, ExprAST *body)
    : Proto(proto), Body(body) {}
};
```

In Kaleidoscope, functions are typed with just a count of their arguments. Since all values are double precision floating point, the type of each argument doesn't need to be stored anywhere. In a more aggressive and realistic language, the `ExprAST` class would probably have a type field.

With this scaffolding, we can now talk about parsing expressions and function bodies in Kaleidoscope.


#### 2.2 Parser Basics

Now that we have an AST to build, we need to define the parser code to build it. The idea here is that we want to parse something like `x+y` (which is returned as three tokens by the lexer) into an AST that could be generated with calls like this:

```
ExprAST *X = new VariableExprAST("x");
ExprAST *Y = new VariableExprAST("y");
ExprAST *Result = new BinaryExprAST('+', X, Y);
In order to do this, we'll start by defining some basic helper routines:

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}
```

This implements a simple token buffer around the lexer. This allows us to look one token ahead at what the lexer is returning. Every function in our parser will assume that CurTok is the current token that needs to be parsed.

```
/// Error* - These are little helper functions for error handling.
ExprAST *Error(const char *Str) { fprintf(stderr, "Error: %s\n", Str);return 0;}
PrototypeAST *ErrorP(const char *Str) { Error(Str); return 0; }
FunctionAST *ErrorF(const char *Str) { Error(Str); return 0; }
```

The Error routines are simple helper routines that our parser will use to handle errors. The error recovery in our parser will not be the best and is not particular user-friendly, but it will be enough for our tutorial. These routines make it easier to handle errors in routines that have various return types: they always return null.

With these basic helper functions, we can implement the first piece of our grammar: numeric literals.


#### 2.3 Basic Expression Parsing

We start with numeric literals, because they are the simplest to process. For each production in our grammar, we'll define a function which parses that production. For numeric literals, we have:

```
/// numberexpr ::= number
static ExprAST *ParseNumberExpr() {
  ExprAST *Result = new NumberExprAST(NumVal);
  getNextToken(); // consume the number
  return Result;
}
```

This routine is very simple: it expects to be called when the current token is a tok_number token. It takes the current number value, creates a NumberExprAST node, advances the lexer to the next token, and finally returns.

There are some interesting aspects to this. The most important one is that this routine eats all of the tokens that correspond to the production and returns the lexer buffer with the next token (which is not part of the grammar production) ready to go. This is a fairly standard way to go for recursive descent parsers. For a better example, the parenthesis operator is defined like this:

```
/// parenexpr ::= '(' expression ')'
static ExprAST *ParseParenExpr() {
  getNextToken();  // eat (.
  ExprAST *V = ParseExpression();
  if (!V) return 0;

  if (CurTok != ')')
    return Error("expected ')'");
  getNextToken();  // eat ).
  return V;
}
```

This function illustrates a number of interesting things about the parser:

1) It shows how we use the Error routines. When called, this function expects that the current token is a `(` token, but after parsing the subexpression, it is possible that there is no `)` waiting. For example, if the user types in `(4 x` instead of `(4)`, the parser should emit an error. Because errors can occur, the parser needs a way to indicate that they happened: in our parser, we return null on an error.

2) Another interesting aspect of this function is that it uses recursion by calling ParseExpression (we will soon see that ParseExpression can call ParseParenExpr). This is powerful because it allows us to handle recursive grammars, and keeps each production very simple. Note that parentheses do not cause construction of AST nodes themselves. While we could do it this way, the most important role of parentheses are to guide the parser and provide grouping. Once the parser constructs the AST, parentheses are not needed.

The next simple production is for handling variable references and function calls:

```
/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static ExprAST *ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken();  // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return new VariableExprAST(IdName);

  // Call.
  getNextToken();  // eat (
  std::vector<ExprAST*> Args;
  if (CurTok != ')') {
    while (1) {
      ExprAST *Arg = ParseExpression();
      if (!Arg) return 0;
      Args.push_back(Arg);

      if (CurTok == ')') break;

      if (CurTok != ',')
        return Error("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return new CallExprAST(IdName, Args);
}
```

This routine follows the same style as the other routines. (It expects to be called if the current token is a tok_identifier token). It also has recursion and error handling. One interesting aspect of this is that it uses look-ahead to determine if the current identifier is a stand alone variable reference or if it is a function call expression. It handles this by checking to see if the token after the identifier is a `(` token, constructing either a VariableExprAST or CallExprAST node as appropriate.

Now that we have all of our simple expression-parsing logic in place, we can define a helper function to wrap it together into one entry point. We call this class of expressions "primary" expressions, for reasons that will become more clear later in the tutorial. In order to parse an arbitrary primary expression, we need to determine what sort of expression it is:

```
/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static ExprAST *ParsePrimary() {
  switch (CurTok) {
  default: return Error("unknown token when expecting an expression");
  case tok_identifier: return ParseIdentifierExpr();
  case tok_number:     return ParseNumberExpr();
  case '(':            return ParseParenExpr();
  }
}
```

Now that you see the definition of this function, it is more obvious why we can assume the state of CurTok in the various functions. This uses look-ahead to determine which sort of expression is being inspected, and then parses it with a function call.

Now that basic expressions are handled, we need to handle binary expressions. They are a bit more complex.


#### 2.4 Binary Expression Parsing

Binary expressions are significantly harder to parse because they are often ambiguous. For example, when given the string `x+y*z`, the parser can choose to parse it as either `(x+y)*z` or `x+(y*z)`. With common definitions from mathematics, we expect the later parse, because `*` (multiplication) has higher precedence than `+` (addition).

There are many ways to handle this, but an elegant and efficient way is to use Operator-Precedence Parsing. This parsing technique uses the precedence of binary operators to guide recursion. To start with, we need a table of precedences:

```
/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0) return -1;
  return TokPrec;
}

int main() {
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40;  // highest.
  ...
}
```

For the basic form of Kaleidoscope, we will only support 4 binary operators (this can obviously be extended by you, our brave and intrepid reader). The GetTokPrecedence function returns the precedence for the current token, or -1 if the token is not a binary operator. Having a map makes it easy to add new operators and makes it clear that the algorithm doesn't depend on the specific operators involved, but it would be easy enough to eliminate the map and do the comparisons in the GetTokPrecedence function. (Or just use a fixed-size array).

With the helper above defined, we can now start parsing binary expressions. The basic idea of operator precedence parsing is to break down an expression with potentially ambiguous binary operators into pieces. Consider ,for example, the expression `a+b+(c+d)*e*f+g`. Operator precedence parsing considers this as a stream of primary expressions separated by binary operators. As such, it will first parse the leading primary expression `a`, then it will see the pairs [+, b] [+, (c+d)] [\*, e] [\*, f] and [+, g]. Note that because parentheses are primary expressions, the binary expression parser doesn't need to worry about nested subexpressions like (c+d) at all.

To start, an expression is a primary expression potentially followed by a sequence of [binop,primaryexpr] pairs:

```
/// expression
///   ::= primary binoprhs
///
static ExprAST *ParseExpression() {
  ExprAST *LHS = ParsePrimary();
  if (!LHS) return 0;

  return ParseBinOpRHS(0, LHS);
}
```

ParseBinOpRHS is the function that parses the sequence of pairs for us. It takes a precedence and a pointer to an expression for the part that has been parsed so far. Note that `x` is a perfectly valid expression: As such, `binoprhs` is allowed to be empty, in which case it returns the expression that is passed into it. In our example above, the code passes the expression for `a` into ParseBinOpRHS and the current token is `+`.

The precedence value passed into ParseBinOpRHS indicates the minimal operator precedence that the function is allowed to eat. For example, if the current pair stream is [+, x] and ParseBinOpRHS is passed in a precedence of 40, it will not consume any tokens (because the precedence of `+` is only 20). With this in mind, ParseBinOpRHS starts with:

```
/// binoprhs
///   ::= ('+' primary)*
static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
  // If this is a binop, find its precedence.
  while (1) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;
```

This code gets the precedence of the current token and checks to see if if is too low. Because we defined invalid tokens to have a precedence of -1, this check implicitly knows that the pair-stream ends when the token stream runs out of binary operators. If this check succeeds, we know that the token is a binary operator and that it will be included in this expression:

```
// Okay, we know this is a binop.
int BinOp = CurTok;
getNextToken();  // eat binop

// Parse the primary expression after the binary operator.
ExprAST *RHS = ParsePrimary();
if (!RHS) return 0;
```

As such, this code eats (and remembers) the binary operator and then parses the primary expression that follows. This builds up the whole pair, the first of which is [+, b] for the running example.

Now that we parsed the left-hand side of an expression and one pair of the RHS sequence, we have to decide which way the expression associates. In particular, we could have `(a+b) binop unparsed` or `a + (b binop unparsed)`. To determine this, we look ahead at `binop` to determine its precedence and compare it to BinOp's precedence (which is `+` in this case):

```
// If BinOp binds less tightly with RHS than the operator after RHS, let
// the pending operator take RHS as its LHS.
int NextPrec = GetTokPrecedence();
if (TokPrec < NextPrec) {
```

If the precedence of the binop to the right of "RHS" is lower or equal to the precedence of our current operator, then we know that the parentheses associate as `(a+b) binop ...`. In our example, the current operator is `+` and the next operator is `+`, we know that they have the same precedence. In this case we'll create the AST node for `a+b`, and then continue parsing:

```
      ... if body omitted ...
    }

    // Merge LHS/RHS.
    LHS = new BinaryExprAST(BinOp, LHS, RHS);
  }  // loop around to the top of the while loop.
}
```

In our example above, this will turn `a+b+` into `(a+b)` and execute the next iteration of the loop, with `+` as the current token. The code above will eat, remember, and parse `(c+d)` as the primary expression, which makes the current pair equal to [+, (c+d)]. It will then evaluate the `if` conditional above with `*` as the binop to the right of the primary. In this case, the precedence of `*` is higher than the precedence of `+` so the if condition will be entered.

The critical question left here is "how can the if condition parse the right hand side in full"? In particular, to build the AST correctly for our example, it needs to get all of `(c+d)*e*f` as the RHS expression variable. The code to do this is surprisingly simple (code from the above two blocks duplicated for context):

```
    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec+1, RHS);
      if (RHS == 0) return 0;
    }
    // Merge LHS/RHS.
    LHS = new BinaryExprAST(BinOp, LHS, RHS);
  }  // loop around to the top of the while loop.
}
```

At this point, we know that the binary operator to the RHS of our primary has higher precedence than the binop we are currently parsing. As such, we know that any sequence of pairs whose operators are all higher precedence than `+` should be parsed together and returned as "RHS". To do this, we recursively invoke the ParseBinOpRHS function specifying "TokPrec+1" as the minimum precedence required for it to continue. In our example above, this will cause it to return the AST node for `(c+d)*e*f` as RHS, which is then set as the RHS of the `+` expression.

Finally, on the next iteration of the while loop, the `+g` piece is parsed and added to the AST. With this little bit of code (14 non-trivial lines), we correctly handle fully general binary expression parsing in a very elegant way. This was a whirlwind tour of this code, and it is somewhat subtle. I recommend running through it with a few tough examples to see how it works.

This wraps up handling of expressions. At this point, we can point the parser at an arbitrary token stream and build an expression from it, stopping at the first token that is not part of the expression. Next up we need to handle function definitions, etc.


#### 2.5 Parsing the Rest

The next thing missing is handling of function prototypes. In Kaleidoscope, these are used both for `extern` function declarations as well as function body definitions. The code to do this is straight-forward and not very interesting (once you've survived expressions):

```
/// prototype
///   ::= id '(' id* ')'
static PrototypeAST *ParsePrototype() {
  if (CurTok != tok_identifier)
    return ErrorP("Expected function name in prototype");

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(')
    return ErrorP("Expected '(' in prototype");

  // Read the list of argument names.
  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return ErrorP("Expected ')' in prototype");

  // success.
  getNextToken();  // eat ')'.

  return new PrototypeAST(FnName, ArgNames);
}
```

Given this, a function definition is very simple, just a prototype plus an expression to implement the body:

```
/// definition ::= 'def' prototype expression
static FunctionAST *ParseDefinition() {
  getNextToken();  // eat def.
  PrototypeAST *Proto = ParsePrototype();
  if (Proto == 0) return 0;

  if (ExprAST *E = ParseExpression())
    return new FunctionAST(Proto, E);
  return 0;
}
```

In addition, we support `extern` to declare functions like `sin` and `cos` as well as to support forward declaration of user functions. These ‘extern's are just prototypes with no body:

```
/// external ::= 'extern' prototype
static PrototypeAST *ParseExtern() {
  getNextToken();  // eat extern.
  return ParsePrototype();
}
```

Finally, we'll also let the user type in arbitrary top-level expressions and evaluate them on the fly. We will handle this by defining anonymous nullary (zero argument) functions for them:

```
/// toplevelexpr ::= expression
static FunctionAST *ParseTopLevelExpr() {
  if (ExprAST *E = ParseExpression()) {
    // Make an anonymous proto.
    PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
    return new FunctionAST(Proto, E);
  }
  return 0;
}
```

Now that we have all the pieces, let's build a little driver that will let us actually execute this code we've built!


#### 2.6 The Driver

The driver for this simply invokes all of the parsing pieces with a top-level dispatch loop. There isn't much interesting here, so I'll just include the top-level loop. See below for full code in the "Top-Level Parsing" section.

```
/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (1) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:    return;
    case ';':        getNextToken(); break;  // ignore top-level semicolons.
    case tok_def:    HandleDefinition(); break;
    case tok_extern: HandleExtern(); break;
    default:         HandleTopLevelExpression(); break;
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