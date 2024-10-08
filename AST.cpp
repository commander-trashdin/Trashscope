#include "AST.h"
#include "errors.h"

llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(AST::getContext(), llvm::APFloat(Val));
}

llvm::Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  llvm::AllocaInst *V = AST::getNamedValues()[Name];
  if (!V)
    LogErrorV("Unknown variable name");

  return AST::getBuilder().CreateLoad(V->getAllocatedType(), V, Name.c_str());
}

llvm::Value *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // This assume we're building without RTTI because LLVM builds that way by
    // default. If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    auto *LHSE = static_cast<VariableExprAST *>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");

    // Codegen the RHS.
    llvm::Value *Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    llvm::Value *Variable = getNamedValues()[LHSE->getName()];
    if (!Variable)
      return LogErrorV("Unknown variable name");

    getBuilder().CreateStore(Val, Variable);
    return Val;
  }

  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return AST::getBuilder().CreateFAdd(L, R, "addtmp");
  case '-':
    return AST::getBuilder().CreateFSub(L, R, "subtmp");
  case '*':
    return AST::getBuilder().CreateFMul(L, R, "multmp");
  case '<':
    L = AST::getBuilder().CreateFCmpULT(L, R, "cmptmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return AST::getBuilder().CreateUIToFP(
        L, llvm::Type::getDoubleTy(AST::getContext()), "booltmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}

llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  llvm::Function *CalleeF = AST::getModule().getFunction(Callee);
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

  return AST::getBuilder().CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<llvm::Type *> Doubles(Args.size(),
                                    llvm::Type::getDoubleTy(getContext()));
  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::Type::getDoubleTy(getContext()), Doubles, false);

  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, Name, getModule());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}

llvm::Function *FunctionAST::codegen() {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function *TheFunction = getModule().getFunction(Proto->getName());

  if (!TheFunction)
    TheFunction = Proto->codegen();

  if (!TheFunction)
    return nullptr;

  if (!TheFunction->empty())
    return (llvm::Function *)LogErrorV("Function cannot be redefined.");

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(getContext(), "entry", TheFunction);
  getBuilder().SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  getNamedValues().clear();
  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable.
    llvm::AllocaInst *Alloca =
        CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()));

    // Store the initial value into the alloca.
    getBuilder().CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    getNamedValues()[std::string(Arg.getName())] = Alloca;
  }

  if (llvm::Value *RetVal = Body->codegen()) {
    // Finish off the function.
    getBuilder().CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    llvm::verifyFunction(*TheFunction);

    Optimizer::getFPM().run(*TheFunction, Optimizer::getFAM());

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

llvm::Value *IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  CondV = AST::getBuilder().CreateFCmpONE(
      CondV, llvm::ConstantFP::get(AST::getContext(), llvm::APFloat(0.0)),
      "ifcond");

  llvm::Function *ThisFunction =
      AST::getBuilder().GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(AST::getContext(), "then", ThisFunction);
  llvm::BasicBlock *ElseBB =
      llvm::BasicBlock::Create(AST::getContext(), "else");
  llvm::BasicBlock *MergeBB =
      llvm::BasicBlock::Create(AST::getContext(), "ifcont");

  AST::getBuilder().CreateCondBr(CondV, ThenBB, ElseBB);

  AST::getBuilder().SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;

  AST::getBuilder().CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = AST::getBuilder().GetInsertBlock();

  // Emit else block.
  ThisFunction->insert(ThisFunction->end(), ElseBB);
  AST::getBuilder().SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen();
  if (!ElseV)
    return nullptr;

  AST::getBuilder().CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = AST::getBuilder().GetInsertBlock();

  ThisFunction->insert(ThisFunction->end(), MergeBB);
  AST::getBuilder().SetInsertPoint(MergeBB);
  llvm::PHINode *PN = AST::getBuilder().CreatePHI(
      llvm::Type::getDoubleTy(AST::getContext()), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

llvm::Value *ForExprAST::codegen() {
  // Make the new basic block for the loop header, inserting after current
  // block.
  llvm::Function *TheFunction = getBuilder().GetInsertBlock()->getParent();
  // Create an alloca for the variable in the entry block.
  llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

  llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(getContext(), "loop", TheFunction);

  // Emit the start code first, without 'variable' in scope.
  llvm::Value *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;
  getBuilder().CreateStore(StartVal, Alloca);

  // Insert an explicit fall through from the current block to the LoopBB.
  getBuilder().CreateBr(LoopBB);

  // Start insertion in LoopBB.
  AST::getBuilder().SetInsertPoint(LoopBB);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  llvm::AllocaInst *OldVal = getNamedValues()[VarName];
  getNamedValues()[VarName] = Alloca;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen())
    return nullptr;

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

  // Compute the end condition.
  llvm::Value *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;

  // Reload, increment, and restore the alloca.  This handles the case where
  // the body of the loop mutates the variable.
  llvm::Value *CurVar = getBuilder().CreateLoad(Alloca->getAllocatedType(),
                                                Alloca, VarName.c_str());
  llvm::Value *NextVar = getBuilder().CreateFAdd(CurVar, StepVal, "nextvar");
  getBuilder().CreateStore(NextVar, Alloca);

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = getBuilder().CreateFCmpONE(
      EndCond, llvm::ConstantFP::get(getContext(), llvm::APFloat(0.0)),
      "loopcond");

  // Create the "after loop" block and insert it.
  llvm::BasicBlock *AfterBB =
      llvm::BasicBlock::Create(getContext(), "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  // br i1 %loopcond, label %loop, label %afterloop
  getBuilder().CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  getBuilder().SetInsertPoint(AfterBB);

  // Restore the unshadowed variable.
  if (OldVal)
    getNamedValues()[VarName] = OldVal;
  else
    getNamedValues().erase(VarName);

  // for expr always returns 0.0.
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(getContext()));
}

llvm::Value *VarExprAST::codegen() {
  std::vector<llvm::AllocaInst *> OldBindings;

  llvm::Function *TheFunction = getBuilder().GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (auto &Var : VarNames) {
    const std::string &VarName = Var.first;
    ExprAST *Init = Var.second.get();

    // Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff
    // like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    llvm::Value *InitVal;

    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    } else { // If not specified, use 0.0.
      InitVal = llvm::ConstantFP::get(getContext(), llvm::APFloat(0.0));
    }

    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    getBuilder().CreateStore(InitVal, Alloca);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(getNamedValues()[VarName]);

    // Remember this binding.
    getNamedValues()[VarName] = Alloca;
  }

  // Codegen the body, now that all vars are in scope.
  llvm::Value *BodyVal = Body->codegen();
  if (!BodyVal)
    return nullptr;

  // Pop all our variables from scope.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
    getNamedValues()[VarNames[i].first] = OldBindings[i];

  // Return the body computation.
  return BodyVal;
}