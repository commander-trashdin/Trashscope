#pragma once

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"

class Optimizer {
public:
  static llvm::FunctionPassManager &getFPM();

  static llvm::LoopAnalysisManager &getLAM();

  static llvm::FunctionAnalysisManager &getFAM();

  static llvm::CGSCCAnalysisManager &getCGAM();

  static llvm::ModuleAnalysisManager &getMAM();

  static llvm::PassInstrumentationCallbacks &getPIC();

  static llvm::StandardInstrumentations &getSI();
};

void InitializeModuleAndManagers();