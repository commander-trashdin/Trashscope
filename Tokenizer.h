#include "AST.h"
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>

class Tokenizer {
  // The lexer returns tokens [0-255] if it is an unknown character, otherwise
  // one of these for known things.
public:
  enum TType {
    Eof = -1,

    // commands
    Def = -2,
    Extern = -3,

    // primary
    Identifier = -4,
    Number = -5,
    If = -6,
    Then = -7,
    Else = -8,
    For = -9,
    In = -10,
    Var = -11
  };

  int GetTok();
  int GetNextToken() { return CurTok = GetTok(); }
  std::unique_ptr<ExprAST> ParseNumberExpr();
  std::unique_ptr<ExprAST> ParseParenExpr();
  std::unique_ptr<ExprAST> ParseIdentifierExpr();
  std::unique_ptr<ExprAST> ParsePrimary();
  int GetTokPrecedence();

  std::unique_ptr<ExprAST> ParseExpression();
  std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                         std::unique_ptr<ExprAST> LHS);

  std::unique_ptr<PrototypeAST> ParsePrototype();
  std::unique_ptr<FunctionAST> ParseDefinition();
  std::unique_ptr<PrototypeAST> ParseExtern();
  std::unique_ptr<FunctionAST> ParseTopLevelExpr();
  std::unique_ptr<ExprAST> ParseIfExpr();
  std::unique_ptr<ExprAST> ParseForExpr();
  std::unique_ptr<ExprAST> ParseVarExpr();

  void HandleDefinition();
  void HandleExtern();
  void HandleTopLevelExpression();

  void MainLoop();

  int &operator[](std::size_t idx) { return BinopPrecedence[idx]; }

  static std::unique_ptr<Tokenizer> &GetInstance();

protected:
  Tokenizer() { GetNextToken(); }

  static std::unique_ptr<Tokenizer> singleton_;

private:
  int LastChar = ' ';
  std::string IdentifierStr; // Filled in if tok_identifier
  double NumVal;             // Filled in if tok_number
  int CurTok;
  std::map<char, int> BinopPrecedence;
};
