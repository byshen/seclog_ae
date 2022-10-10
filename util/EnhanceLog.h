#ifndef ENHANCELOG_H
#define ENHANCELOG_H

#include "../Utils.h"
#include "../MyCallGraph.h"

#include "llvm-c/Core.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

// currently, we ask developer to provide a function like printf
// the name is ace_printf

string aceLogFuncName = "ace_printf";
static int globalLogID = 0;

extern MiniCallGraph *g_callGraph;

// #define INSERT_LOG
// #define REMOVE_EXISTING_LOG

Function* getLogFunction(string name) {
  MiniGraphNode *graphNode = g_callGraph->getCallGraphNodeByName(name);
  if (graphNode) {
    return graphNode->_function;
  }
  return nullptr;
}

// This convert the index sliced in the AC function,
// to the variables at the callsite
void getSlicedCallArgs(Instruction *instr, set<ParamPos> &argints,
                       map<Value *, pair<Value *, int>>
                           &origianlParams) // <originalArg, allocaArg>
{

  CallInst *callInst = dyn_cast<CallInst>(instr);
  if (callInst == nullptr)
    LOGERR("call site is nullptr!!!\n");

  // do the ugly hack for the library calls
  // this is because the library like apr_file_open() always do
  // not slice any related function parameters, then
  // we assume all of the parameters are related.
  if (argints.size() == 0) {
    for (unsigned i = 0; i < callInst->getNumArgOperands(); ++i)
      argints.insert(make_pair(i, -1));
  }

  for (auto &idx : argints) {
    // LOGERR("[DEBUG]", idx.first, "\t", idx.second, "\t", callInst->getNumArgOperands(), "\n");
    if (idx.first < 0 || (unsigned)idx.first >= callInst->getNumArgOperands()) {
      LOGERR("call site has invalid sliced argument index!!!\n");
      continue; // skip
    }
    Value *loadValue = callInst->getArgOperand((unsigned)idx.first);
    origianlParams[loadValue] = make_pair(nullptr, -1);

    if (LoadInst *loadInst = dyn_cast<LoadInst>(loadValue)) {
      origianlParams[loadValue] = make_pair(
          loadInst->getOperand(0), idx.second); // this is the real source!!!
    } else if (GetElementPtrInst *gepInst =
                   dyn_cast<GetElementPtrInst>(loadValue)) {
      // we do the best effort here, if it may be an array
      // %12 = getelementptr inbounds [4 x i8], [4 x i8]* %msg, i32 0, i32 0,
      // !dbg !11
      origianlParams[loadValue] = make_pair(gepInst->getOperand(0), idx.second);
    } else {
      origianlParams[loadValue] = make_pair(loadValue, idx.second);
    }
  }
}

// deprecated, we directly get the log function from the bitcode now
// see Function* getLogFunction(string name);
void insertGlobalLogFunction(Module *m) {
  // Function *CalleeF = m->getOrInsertFunction("printf",
  //     FunctionType::get(IntegerType::getInt32Ty(Context),
  //     PointerType::get(Type::getInt8Ty(Context), 0),
  //     true /* this is var arg func type*/)
  //     );
}

const Function *findEnclosingFunc(const Value *V) {
  if (const Argument *Arg = dyn_cast<Argument>(V)) {
    return Arg->getParent();
  }
  if (const Instruction *I = dyn_cast<Instruction>(V)) {
    return I->getParent()->getParent();
  }
  return NULL;
}

const DILocalVariable *findVar(const Value *V, const Function *F) {
  for (const_inst_iterator Iter = inst_begin(F), End = inst_end(F); Iter != End;
       ++Iter) {
    const Instruction *I = &*Iter;
    if (const DbgDeclareInst *DbgDeclare = dyn_cast<DbgDeclareInst>(I)) {
      if (DbgDeclare->getAddress() == V)
        return DbgDeclare->getVariable();
    } else if (const DbgValueInst *DbgValue = dyn_cast<DbgValueInst>(I)) {
      if (DbgValue->getValue() == V)
        return DbgValue->getVariable();
    }
  }
  return NULL;
}

StringRef getOriginalName(const Value *V) {
  // TODO handle globals as well

  const Function *F = findEnclosingFunc(V);
  if (!F)
    return V->getName();

  const DILocalVariable *Var = findVar(V, F);
  if (!Var)
    return "unknown_var";

  return Var->getName();
}

string getVarValueInString(Value *v) {
  // Value;
  if (v == nullptr)
    return "-1";
  LOGERR("[value]\t", *v, "\n");
  if (v->getType() == nullptr) {
    LOGERR("[value type is null]\t", *v, "\n");
    return "-1";
  }

  // First, if it is a global value!!!
  if (GlobalVariable *G = dyn_cast<GlobalVariable>(v)) {
    errs() << "Getting Global value type! " << *G << "\n";
    if (G->getType()->isPointerTy()) { // all globals are actually pointers
      Type *pointeeTy = G->getType()->getPointerElementType();

      if (pointeeTy->isIntegerTy()) {
        Constant *constValue = G->getInitializer();
        ConstantInt *constInt = cast<ConstantInt>(constValue);
        int64_t constIntValue = constInt->getSExtValue();
        LOGERR("Global int:", constIntValue, "\n");
        return "%d";
      } else if (pointeeTy->isArrayTy()) { // @msg = global [4 x i8] c"new\00",
                                           // align 1, !dbg !0

        auto constArray = dyn_cast<ConstantDataArray>(G->getInitializer());
        if (constArray) {
          // LOGERR("pointer type constant array",
          // pointeeTy->getIntegerBitWidth(), "\n");
          if (constArray->isString()) {
            LOGERR("Global string", constArray->getAsCString().str(), "\n");
            return "%s";
          }
        } else {
          return "-1";
        }
      }
    }

    return "-1"; // end of GV return
  }

  if (v->getType()->isAggregateType()) {
    LOGERR("Can not log struct or array\n");
    return "-1";
  }

  if (v->getType()->isFloatingPointTy()) {
    return "%f";
  }

  if (v->getType()->isIntegerTy()) {
    return "%d";
  }

  if (v->getType()->isPointerTy()) { // i8*
    Type *pointeeTy = v->getType()->getPointerElementType();

    if (pointeeTy) {
      if (pointeeTy->isStructTy()) {
        // the following instruction will be here;
        // %11 = load %struct.request*, %struct.request** %a, align 8, !dbg !34
        LOGERR("Can not log struct or array\n");
        return "-1";
      }
      if (pointeeTy->isIntegerTy()) {
        if (pointeeTy->getIntegerBitWidth() == 8)
          return "%s";
      } else {
        LOGERR("pointer with not struct or char", pointeeTy->getTypeID(), "\n");
      }
    }
  }

  LOGERR("UNKNOWN TYPE\n");
  return "-1";
}

string getStructVarValueInString(Value *v, int pos) {
  if (pos < 0)
    return getVarValueInString(v);
  if (v->getType()->isPointerTy()) {
    Type *pointeeTy = v->getType()->getPointerElementType();

    if (pointeeTy) {
      if (pointeeTy->isStructTy()) {
        // the following instruction will be here;
        // %11 = load %struct.request*, %struct.request** %a, align 8, !dbg !34
        Type *realtype = dyn_cast<StructType>(pointeeTy)->getElementType(pos);
        if (realtype->isFloatingPointTy()) {
          return "%f";
        } else if (realtype->isIntegerTy()) {
          return "%d";
        } else if (realtype->isPointerTy()) { // i8*
          Type *realrealType = realtype->getPointerElementType();
          if (realrealType && realrealType->isIntegerTy()) {
            if (realrealType->getIntegerBitWidth() == 8)
              return "%s";
          }
        }
      }
    }
  }
  return "-1";
}

// deprecated
string insertLogInDeniedBranch(Module *m, DeniedBranchContainer *container,
                               map<Value *, pair<Value *, int>> keyVars,
                               // complex here for the access check function
                               // <original callsite Arg, allocaArg, field in
                               // access check function>
                               set<Value *> localVars,
                               set<ParamPos> localFuncArgs) {
  string summary = "";
  string summaryvalues = "";

  BasicBlock *entry = container->getDeniedBasicBlock();
  LLVMContext &context = m->getContext();
  IRBuilder<> builder(entry);

  Instruction *insertPoint = entry->getFirstNonPHI();
  LOGERR("INSERT BEFORE", *insertPoint, "\n");
  builder.SetInsertPoint(insertPoint);

  FunctionType *funcTy =
      FunctionType::get(IntegerType::getInt32Ty(context),
                        PointerType::get(Type::getInt8Ty(context), 0),
                        true /* this is var arg func type*/);

  Function *CalleeF = getLogFunction(aceLogFuncName);
      // cast<Function>(m->getOrInsertFunction(aceLogFuncName, funcTy).getCallee());
  if (CalleeF == nullptr) {
    LOGERR(aceLogFuncName, "not found");
  }

  // create format string;
  string logTemplate = "[A]";
  vector<Value *> vec;

  //////////////////////////////////////////////////////////
  logTemplate += "[]:";
  summaryvalues += "[AC args]";
  for (auto &i : keyVars) {
    // insert like "varname=var_value",
    string varformat = getVarValueInString(i.first);
    StringRef varName = getOriginalName(i.second.first);
    string field = "";
    if (i.second.second >= 0) {
      varformat = getStructVarValueInString(i.first, i.second.second);
      field = ".field." + to_string(i.second.second);
    }
    summaryvalues += varformat + "=" + varName.str() + field + "|";
    if (varformat == "-1") {
      LOGERR("[err!!!] can not translate this value", *i.first, "\n");
      continue;
    }
    // also need to get the var value;
    Value *strValue = builder.CreateGlobalStringPtr(
        varName.str(), "ace.str." + varName.str() + to_string(globalLogID));

    vec.push_back(strValue);
    vec.push_back(i.first);
    logTemplate += ("%s=" + varformat + " ");
  }

  summaryvalues += "[global variables]";
  //////////////////////////////////////////////////////////
  logTemplate += " []:";
  for (auto &i : localVars) {
    string varformat = getVarValueInString(i);
    StringRef varName = i->getName();
    summaryvalues += varformat + "=" + varName.str() + "|";

    if (varformat == "-1") {
      LOGERR("[err!!!] can not translate this value", *i, "\n");
      continue;
    }
    // also need to get the var value;

    Value *strValue = builder.CreateGlobalStringPtr(
        varName.str(), "ace.str." + varName.str() + to_string(globalLogID));

    vec.push_back(strValue);
    if (varformat == "%d") {
      // load the global value; i32
      Value *loadedValue = builder.CreateLoad(
          i->getType()->getPointerElementType(), i,
          "loadInt." + varName.str()); // i is the global value
      vec.push_back(loadedValue);
      ;
    } else {
      vec.push_back(i);
    }
    logTemplate += ("%s=" + varformat + " ");
  }
  summaryvalues += "[caller func args]";
  //////////////////////////////////////////////////////////
  logTemplate += "[]:";
  int pos = 0;

  Function *_func = entry->getParent(); // deny block->getfunction
  for (auto ab = _func->arg_begin(), ae = _func->arg_end(); ab != ae; ++ab) {
    // if (localFuncArgs.find(pos)!= localFuncArgs.end())
    for (auto &parapos : localFuncArgs)
      if (parapos.first == pos) {
        auto abb = &*ab;
        Value *a = dyn_cast<Value>(abb);

        string varformat = getVarValueInString(a);
        StringRef varName = a->getName();
        string field = "";
        if (parapos.second >= 0) {
          varformat = getStructVarValueInString(a, parapos.second);
          field = ".field." + to_string(parapos.first);
        }

        summaryvalues += varformat + "=" + varName.str() + field + "|";
        if (varformat == "-1") {
          LOGERR("[err!!!] can not translate this value", *a, "\n");
          continue;
        }
        // also need to get the var value;
        Value *strValue = builder.CreateGlobalStringPtr(
            varName.str(), "ace.str." + varName.str() + to_string(globalLogID));

        vec.push_back(strValue);
        vec.push_back(a);
        logTemplate += ("%s=" + varformat + " ");
      }

    pos++;
  }

  // logTemplate += "\n";

  Value *str = builder.CreateGlobalStringPtr(
      logTemplate, "ace.template." + to_string(globalLogID));
  vec.insert(vec.begin(), str);

  ArrayRef<Value *> args(vec);

  LOGERR(logTemplate, "\n");
  builder.CreateCall(funcTy, CalleeF, args,
                     "printCall" + to_string(globalLogID));

  globalLogID++;
  // for summary only
  summary = logTemplate.substr(0, logTemplate.size() - 1) + "," +
            summaryvalues + "\n";
  return summary;
}

string getInsertLogOnDenial(Module *m, 
                            Function *caller,
                            Instruction *callInst,
                            DeniedBranchContainer *container,
                            map<Value *, pair<Value *, int>> keyVars,
                            // complex here for the access check function
                            // <original callsite Arg, allocaArg, field in
                            // access check function>
                            set<Value *> localVars,
                            set<ParamPos> localFuncArgs,
                            bool insertCheck = false, // if insertCheck is true, rcf  
                                                      // must *not* be nullptr
                            ResultCheckFunc* rcf = nullptr) 
{
  string summary = "";
  string summaryvalues = "";

  // create format string;
  string logTemplate = "|";
  // format suffix is globalID.var_ID;
  int var_ID = 0;

  LLVMContext &context = m->getContext();
  // dummy values to depress errors
  // should be overwritten if container is not nullptr
  BasicBlock *entry = nullptr;
  IRBuilder<> *builder;

  // creating allca instrs for holding temp values
  BasicBlock *funcEntry = nullptr;
  IRBuilder<> *builderAllca;

  // store the AC args before the ac funccall
  BasicBlock *acEntry = nullptr;
  IRBuilder<> *builderStore;

  // log function parameters
  vector<Value *> logValVec;

  // type of log function - removed since we get the log func from bitcode
  // FunctionType *funcTy;

  // the build function of inserted function call
  Function *CalleeF;

  
#ifdef INSERT_LOG
  if (container != nullptr) {
    entry = container->getDeniedBasicBlock();
    builder = new IRBuilder<>(entry);

    Instruction *insertPoint = entry->getFirstNonPHI();
    LOGERR("INSERT BEFORE", *insertPoint, "\n");
    (*builder).SetInsertPoint(insertPoint);

    // funcTy = FunctionType::get(IntegerType::getInt32Ty(context),
    //                            PointerType::get(Type::getInt8Ty(context), 0),
    //                            true /* this is var arg func type*/);

    // TODO just use printf function for test
    CalleeF = getLogFunction(aceLogFuncName);
        // cast<Function>(m->getOrInsertFunction(aceLogFuncName, funcTy).getCallee());
    if (CalleeF == nullptr) {
      LOGERR(aceLogFuncName, " not found");
    }

    // get the first basic block for us to insert alloca instrs
    funcEntry = & (caller->getEntryBlock());
    builderAllca = new IRBuilder<>(funcEntry);
    (*builderAllca).SetInsertPoint(funcEntry->getFirstNonPHI());

    // store the values to alloca before we insert
    acEntry = callInst->getParent();
    builderStore = new IRBuilder<>(acEntry);
    (*builderStore).SetInsertPoint(callInst); 

  }

  if (insertCheck) {
    BasicBlock* originBB = callInst->getParent();
    LOGDEBUG("Spliting the original BB", *originBB, "\n");
    BasicBlock::iterator I(callInst);
    if (++I == callInst->getParent()->end()){
      LOGERR("WEIRD, This callInst is at the end of basicBlock", *callInst, "\n");
      assert(false);
    }
    
    Instruction* callNextInst = callInst->getNextNode();
    BasicBlock* denyBB = originBB->splitBasicBlock(callNextInst);
    BasicBlock* newBB = SplitEdge(originBB, denyBB);

    builder = new IRBuilder<>(originBB);
    
    builder->SetInsertPoint( originBB->getTerminator() ); // insert before the unconditional jump
    LOGDEBUG("=========", "\n");
    LOGDEBUG(*originBB);
    LOGDEBUG(*denyBB);
    LOGDEBUG(*newBB);
    LOGDEBUG("=========", "\n");

    LOGERR("new unconditional br", *(originBB->getTerminator()) );
    LOGERR(rcf->realRCF->getName(), "\n");
    
    CallInst* checkResult = builder->CreateCall(rcf->realRCF, callInst);
    Value* checkCond = builder->CreateICmpEQ(checkResult, builder->getInt32(1));
    BranchInst* newBr = builder->CreateCondBr(checkCond, denyBB, newBB);

    // delete the unconditional BR
    originBB->getTerminator()->eraseFromParent();


    builder->SetInsertPoint(denyBB);
    // BranchInst* logPoint = builder->CreateBr(newBB);
    builder->SetInsertPoint(denyBB->getTerminator());


    LOGDEBUG("=========", "\n");
    LOGDEBUG(*originBB);
    LOGDEBUG(*denyBB);
    LOGDEBUG(*newBB);
    LOGDEBUG("=========", "\n");

    
    /*
    entry = container->getDeniedBasicBlock();
    builder = new IRBuilder<>(entry);

    Instruction *insertPoint = entry->getFirstNonPHI();
    LOGERR("INSERT BEFORE", *insertPoint, "\n");
    (*builder).SetInsertPoint(insertPoint);
    */
    CalleeF = getLogFunction(aceLogFuncName);
    if (CalleeF == nullptr) {
      LOGERR(aceLogFuncName, " not found");
    }
    // get the first basic block for us to insert alloca instrs
    funcEntry = & (caller->getEntryBlock());
    builderAllca = new IRBuilder<>(funcEntry);
    (*builderAllca).SetInsertPoint(funcEntry->getFirstNonPHI());

    // store the values to alloca before we insert
    acEntry = callInst->getParent();
    builderStore = new IRBuilder<>(acEntry);
    (*builderStore).SetInsertPoint(callInst); 
  }
#endif


  if (insertCheck) {
    logTemplate += "new|";   // even no check branch
  } else {
    if (container != nullptr && container->hasLog) {
      logTemplate += "old|"; // has log
    } 
    else if (container != nullptr && !container->hasLog) {
      logTemplate += "nod|"; // has the branch, but no log
    }
  }
  ///////////////////////////////////////////////////
  // 1. Places to collect the AC function's argument values
  ///////////////////////////////////////////////////
  logTemplate += "1]";
  summaryvalues += "[AC args]";
  for (auto &i : keyVars) {
    // insert like "varname=var_value",
    string varformat = getVarValueInString(i.first);
    StringRef varName = getOriginalName(i.second.first);
    string field = "";
    if (i.second.second >= 0) {
      varformat = getStructVarValueInString(i.first, i.second.second);
      field = ".field." + to_string(i.second.second);
    }
    summaryvalues += varName.str() + field + "=" + varformat + "|";
    if (varformat == "-1") {
      LOGERR("[err!!!] can not translate this value", *i.first, "\n");
    }

    logTemplate += ("%s=" + varformat + " ");

#ifdef INSERT_LOG
    LOGDEBUG("[Inserting variable]", *i.first, "\t",  *i.second.first, "\t",  i.second.second, "\n");
    if (insertCheck || container != nullptr) {
      Value *strValue = (*builder).CreateGlobalStringPtr(
          varName.str(), // this may be "" 
          "ace.str.acarg." + varName.str() + "." + to_string(globalLogID) + "." + to_string(var_ID++));
      logValVec.push_back(strValue);
      // Only need to insert the variable value, when we can log it
      // try the best to reload the previous value used in AC args
      if (varformat != "-1") {

        if (i.second.second < 0) {
          // if (isa<LoadInst>(i.first)) {
          //   LoadInst* ldInst = dyn_cast<LoadInst>(i.first);
          //   Value* val = ldInst->getOperand(0);
          //   auto* newld = (*builder).CreateLoad(val);
          //   logValVec.push_back(newld);
          // }
          // else{
          //   logValVec.push_back(i.first);
          // }
          
          if (varformat == "%d" || varformat == "%f") {
            auto* allocaInst = (*builderAllca).CreateAlloca(
              i.first->getType(), 
              nullptr, 
              "temp."+ to_string(globalLogID) + "." + to_string(var_ID++) );
            (*builderStore).CreateStore(i.first, allocaInst, false);
        
            auto* loadInst = (*builder).CreateLoad(allocaInst);
            logValVec.push_back(loadInst);
          } 
          if (varformat == "%s") {
            auto* allocaInst = (*builderAllca).CreateAlloca(
              i.first->getType(), 
              nullptr,
              "temp."+ to_string(globalLogID) + "." + to_string(var_ID++) );
            (*builderStore).CreateStore(i.first, allocaInst, false);
        
            auto* loadInst = (*builder).CreateLoad(allocaInst);
            logValVec.push_back(loadInst);
          } 

        }
        else {
          // TODO To log the specific fields of specific fields of getelementptr 
          // (*builder).CreateGEP()
        
          logValVec.push_back(i.first);
        }
      }
    }
#endif
  }

  ///////////////////////////////////////////////////
  // 2. Places to collect the global variables
  // TODO remove the duplicates in global variables & function args
  ///////////////////////////////////////////////////
  summaryvalues += "[global variables]";
  logTemplate += "2]:";
  for (auto &i : localVars) {
    string varformat = getVarValueInString(i);
    StringRef varName = i->getName();
    summaryvalues += varName.str() + "=" + varformat + "|";

    if (varformat == "-1") {
      LOGERR("[err!!!] can not translate this value", *i, "\n");
    }

    logTemplate += ("%s=" + varformat + " ");

#ifdef INSERT_LOG
    if (insertCheck || container != nullptr) {

      LOGDEBUG("[Inserting variable]", *i, "\n");

      Value *strValue = (*builder).CreateGlobalStringPtr(
          varName.str(), 
          "ace.str.gb." + varName.str() + "." + to_string(globalLogID) + "." + to_string(var_ID++));

      logValVec.push_back(strValue);
      if (varformat == "%d") {
        // load the global value; i32
        Value *loadedValue = (*builder).CreateLoad(
            i->getType()->getPointerElementType(), i,
            "loadInt." + varName.str()); // i is the global value
        logValVec.push_back(loadedValue);
      } else {
        logValVec.push_back(i);
      }
    }
#endif
  }

  ///////////////////////////////////////////////////
  // 3. Places to collect the args of the caller
  // TODO remove the duplicates in global variables & function args
  ///////////////////////////////////////////////////
  summaryvalues += "[caller func args]";
  logTemplate += "3]:";
  int pos = 0;

  Function *_func = caller; // deny block->getfunction
  for (auto ab = _func->arg_begin(), ae = _func->arg_end(); ab != ae; ++ab) {
    // if (localFuncArgs.find(pos)!= localFuncArgs.end())
    for (auto &parapos : localFuncArgs)
      if (parapos.first == pos) {
        auto abb = &*ab;
        Value *a = dyn_cast<Value>(abb);
                                         
        string varformat = getVarValueInString(a);
        StringRef varName = a->getName();
        string field = "";
        if (parapos.second >= 0) {
          varformat = getStructVarValueInString(a, parapos.second);
          field = ".field." + to_string(parapos.second);
        }

        summaryvalues += varName.str() + field + "=" + varformat + "|";
        if (varformat == "-1") {
          LOGERR("[err!!!] can not translate this value", *a, "\n");
        }

        logTemplate += ("%s=" + varformat + " ");

#ifdef INSERT_LOG
        if (insertCheck || container != nullptr) {
          LOGDEBUG("[Inserting variable]", *a, "\t", parapos.first, "\t", parapos.second, "\n");
          Value *strValue = (*builder).CreateGlobalStringPtr(
              varName.str(),
              "ace.str.callarg." + varName.str()  + "." + to_string(globalLogID)  + "." + to_string(var_ID++));

          logValVec.push_back(strValue);
          if (varformat != "-1") {
            logValVec.push_back(a);
          }
        }
#endif
      }

    pos++;
  }


  // logTemplate += "\n";
  LOGERR(logTemplate, "\n");

#ifdef INSERT_LOG
  // TODO how to avoid duplicate calls?

  if (insertCheck || container != nullptr) {
    Value *str = (*builder).CreateGlobalStringPtr(
        logTemplate, "ace.template." + to_string(globalLogID));
    logValVec.insert(logValVec.begin(), str);

    ArrayRef<Value *> args(logValVec);

    LOGERR(logTemplate, "\n");
    (*builder).CreateCall(CalleeF, args);
                          // "printCall" + to_string(globalLogID));
  }
#endif

#ifdef REMOVE_EXISTING_LOG
    // only remove the existing log 
    // this should be enabled only when INSERT_LOG is defined
    if (container != nullptr) {
      if (container->hasLog && ! container->isLogAtUpperLevel) {
        Instruction* logInstr = container->getLogInstruction();
        LOGINFO("Erasing exisiting log function at,", *logInstr, "\n");
        logInstr->removeFromParent();
      } 
    }
#endif
  // LOGDEBUG("=========", "\n");
  // LOGDEBUG(*(caller));
  // LOGDEBUG("=========", "\n");
  

  globalLogID++;
  // for summary only
  summary = logTemplate.substr(0, logTemplate.size() - 1) + "," +
            summaryvalues + "\n";

  return summary;
}

#endif // ENHANCELOG_H
