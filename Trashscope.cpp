#include "AST.h"
#include "Optimizer.h"
#include "Tokenizer.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include <llvm/Support/FileSystem.h>
#include <llvm/TargetParser/Host.h>

int genObjectFile() {
  auto TargetTriple = llvm::sys::getDefaultTargetTriple();

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  std::string Error;
  auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    llvm::errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features,
                                                   opt, llvm::Reloc::PIC_);

  AST::getModule().setDataLayout(TargetMachine->createDataLayout());
  AST::getModule().setTargetTriple(TargetTriple);

  auto Filename = "output.o";
  std::error_code EC;
  llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message();
    return 1;
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::CodeGenFileType::ObjectFile;

  if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    llvm::errs() << "TargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(AST::getModule());
  dest.flush();
  return 0;
}

int main() {
  auto tok = *Tokenizer::GetInstance();
  // Install standard binary operators.
  // 1 is lowest precedence.
  tok['='] = 2;
  tok['<'] = 10;
  tok['+'] = 20;
  tok['-'] = 20;
  tok['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  InitializeModuleAndManagers();

  // Run the main "interpreter loop" now.
  tok.MainLoop();
  int err = genObjectFile();
  if (err == 1)
    return 1;
  return 0;
}