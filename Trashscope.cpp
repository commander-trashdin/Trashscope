#include "AST.h"
#include "Tokenizer.h"

int main() {
  auto tok = *Tokenizer::GetInstance();
  // Install standard binary operators.
  // 1 is lowest precedence.
  tok['<'] = 10;
  tok['+'] = 20;
  tok['-'] = 20;
  tok['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");

  // Run the main "interpreter loop" now.
  tok.MainLoop();

  // Print out all of the generated code.
  AST::getModule().print(llvm::errs(), nullptr);

  return 0;
}