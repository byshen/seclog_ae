#ifndef LLVMPASS_COMMON_H
#define LLVMPASS_COMMON_H
#include "Utils.h"
#include "llvm/IR/DebugInfoMetadata.h"

/* LLVM has intrinsic functions. Generally we want to ignore
 * them, but sometimes we have to deal with them. For example,
 * LLVM uses its llvm.memset.p0i8.i32() for memset(). And we
 * should care for them so that we don't miss memset()...
 * Return Value:
 * 		true if the stripped function name is interesting for us
 * 		false if we really want to ignore it
 * Parameters:
 * 		Function* func:
 */

static const int NULLPTR_VAL = 4399;

bool stripIntrinsic(Function *func, StringRef &strippedName) {
  /*If it is not intrinsic at all, return true*/
  if (!func->isIntrinsic()) {
    strippedName = func->getName();
    return true;
  }
  /* else check the list of interesting function names
   * that may be prefixed by "llvm"...
   */
  StringRef funcName = func->getName();
  if (funcName.find(".memset.") != StringRef::npos) {
    strippedName = "memset";
    return true;

  } else if (funcName.find(".memcpy.") != StringRef::npos) {
    strippedName = "memcpy";
    return true;
  }
  return false;
}

Function *getCalledFunction(CallInst *callInst) {
  Function *callee = callInst->getCalledFunction();

  if (callee) {
    return callee;

  } else { // damn it, it's either a combined CallInst, or a function pointer
    // errs() << "[error]" << "combined CallInst or Function pointer" <<
    // *callInst << "\n";
    for (unsigned i = 0; i < callInst->getNumOperands(); i++) {
      Value *v = callInst->getOperand(i);

      if (!v) {
        return NULL;
      }

      if (isa<ConstantExpr>(v)) {
        ConstantExpr *cExpr = dyn_cast<ConstantExpr>(v);

        if (cExpr->isCast()) {
          Value *mf = cExpr->getOperand(0); // maybe func?

          if (isa<Function>(mf)) {
            Function *f = dyn_cast<Function>(mf);
            return f;

          } else {
            errs()
                << "[error] undefined cases: the first operand is zero! omg\n";
            //						errs() << "        " <<
            //*callInst
            //<<
            //"\n";
            return NULL;
          }

        } else {
          //					errs() << "[error] undefined
          // cases: not a cast!"
          //<< *callInst << "\n";
          errs() << "[error] undefined cases: not a cast!\n";
          return NULL;
        }
      }
    }
    // probably function pointers, or like this:
    //%0 = call { i32, i32 } asm sideeffect "rdtsc",
    //"={ax},={dx},~{dirflag},~{fpsr},~{flags}"() nounwind, !dbg !3667763,
    //! srcloc !3667764 		errs() << "[error] undefined cases: not
    //! a ConstantExpr!"
    //<< *callInst << "\n";
    errs() << "[error] undefined cases: not a ConstantExpr!\n";
    //		errs() << *callInst << "\n";

    return NULL;
  }

  return NULL;
}

bool hasLogFunc(set<string> logFuncs, BasicBlock *bb, Instruction **logInstr) {
  bool containLogFunc = false;
  if (bb == nullptr)
    return false;
  for (BasicBlock::iterator II = bb->begin(); II != bb->end(); ++II) {
    if (isa<CallInst>(II) || isa<InvokeInst>(II)) {
      Function *callee;
      if (isa<CallInst>(II)) {
        CallInst *callinst = dyn_cast<CallInst>(II);
        callee = getCalledFunction(callinst);

      } else if (isa<InvokeInst>(II)) {
        InvokeInst *invokeinst = dyn_cast<InvokeInst>(II);
        callee = invokeinst->getCalledFunction();
      }

      StringRef realName;
      // ignore all the intrinsic function calls
      if (!callee || !stripIntrinsic(callee, realName)) {
        continue;
      }
      // only insert when callee is present
      // and when callee is not a exit func
      // errs() << "Found log function " << callee->getName() << "\n";

      if (logFuncs.find(callee->getName().str()) != logFuncs.end()) {
        containLogFunc = true;
        *logInstr = &*II;
        if (logInstr != nullptr)
          LOGINFO("Found log function in this instruction ", *II, "\n");
        continue;
      }
    }
  }
  return containLogFunc;
}

/* Get the source code location of a specific instruction.
 * This can be put into utility class later, if we will have that class:)
 * Input:
 * 		Instruction* inst: the instruction we target on
 * Output:
 * 		unsigned& linenum: the linenumber of the instruction
 * 		StringRef& filename: the file name of the source code
 */
void getInstSrcLocation(Instruction *inst, unsigned &linenum,
                        StringRef &filename) {
  MDNode *dbg = inst->getMetadata(LLVMContext::MD_dbg);
  if (dbg == nullptr) {
    LOGERR("\nthis is bad\n");
    return;
  }
  DILocation *loc = dyn_cast<DILocation>(dbg);
  if (loc == nullptr) {
    return;
    LOGERR("\nthis is bad cast\n");
  }
  linenum = loc->getLine();
  filename = loc->getFilename();
}

#endif // LLVMPASS_COMMON_H
