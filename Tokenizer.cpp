#include "Tokenizer.h"
#include "errors.h"

std::unique_ptr<Tokenizer> Tokenizer::singleton_ = nullptr;

int Tokenizer::GetTok() {
  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;

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
    if (IdentifierStr == "for")
      return For;
    if (IdentifierStr == "in")
      return In;
    if (IdentifierStr == "var")
      return Var;

    return Identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return Number;
  }

  if (LastChar == '#') {
    // Comment until end of line.
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return GetTok();
  }
  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return Eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

std::unique_ptr<Tokenizer> &Tokenizer::GetInstance() {
  if (singleton_ == nullptr) {
    singleton_ = std::unique_ptr<Tokenizer>(new Tokenizer());
  }
  return singleton_;
}

/// numberexpr ::= number
std::unique_ptr<ExprAST> Tokenizer::ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  GetNextToken(); // consume the number
  return std::move(Result);
}

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
  case Var:
    return ParseVarExpr();

  default:
    return LogError("unknown token when expecting an expression");
  }
}

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

std::unique_ptr<ExprAST> Tokenizer::ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

std::unique_ptr<ExprAST>
Tokenizer::ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    int BinOp = CurTok;
    GetNextToken(); // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

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
  } // loop around to the top of the while loop.
}

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

std::unique_ptr<FunctionAST> Tokenizer::ParseDefinition() {
  GetNextToken(); // eat def.
  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

std::unique_ptr<PrototypeAST> Tokenizer::ParseExtern() {
  GetNextToken(); // eat extern.
  return ParsePrototype();
}

std::unique_ptr<FunctionAST> Tokenizer::ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

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

std::unique_ptr<ExprAST> Tokenizer::ParseVarExpr() {
  GetNextToken(); // eat the var.

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;

  // At least one variable name is required.
  if (CurTok != Identifier)
    return LogError("expected identifier after var");

  while (true) {
    std::string Name = IdentifierStr;
    GetNextToken(); // eat identifier.

    // Read the optional initializer.
    std::unique_ptr<ExprAST> Init;
    if (CurTok == '=') {
      GetNextToken(); // eat the '='.

      Init = ParseExpression();
      if (!Init)
        return nullptr;
    }

    VarNames.emplace_back(Name, std::move(Init));

    // End of var list, exit loop.
    if (CurTok != ',')
      break;
    GetNextToken(); // eat the ','.

    if (CurTok != Identifier)
      return LogError("expected identifier list after var");
  }

  // At this point, we have to have 'in'.
  if (CurTok != In)
    return LogError("expected 'in' keyword after 'var'");
  GetNextToken(); // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}

void Tokenizer::HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

void Tokenizer::HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

void Tokenizer::HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read top-level expression:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");

      // Remove the anonymous expression.
      FnIR->eraseFromParent();
    }
  } else {
    // Skip token for error recovery.
    GetNextToken();
  }
}

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
