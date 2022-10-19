// Access control check function analyzer

#ifndef _ACFuncAnalyzer_H_
#define _ACFuncAnalyzer_H_

// #define AC_FUNC_ANALYZER_DEBUG

#include "Common.h"
#include "DataDependencyAnalyzer.h" // TaintAnalysis
#include "LogicRelationAnalysis.h"
#include "Struct.h"
#include "Utils.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <algorithm>
// #include "DGFuncAnalyzer.h"
#include "DepGraph.h"
#include <chrono>

#include "util/Container.h"
#include "util/EnhanceLog.h"

// global call_graph
extern MiniCallGraph *g_callGraph;

enum ACFuncAnalysisType {
  CNT_BASIC_BLOCK,
  CNT_INSTRUCTION,
  CNT_FUNCCALL_BLOCK,       // only count including functions
  CNT_FUNCCALL_INSTRUCTION, // count with functions
};

enum ACFuncAnalysisType currAnalysisType = CNT_INSTRUCTION;

struct FunctionProfiler {
  FunctionProfiler(Module &M) { runOnModule(M); }
  ~FunctionProfiler() {
    func2BlockCnt.clear();
    func2InstrCnt.clear();
  }

  map<Function *, int> func2BlockCnt; // function -> num of blocks in function
  map<Function *, int> func2InstrCnt;

  void runOnModule(Module &M) {
    for (auto I = M.begin(), E = M.end(); I != E; ++I) {
      auto function = &*I;
      func2BlockCnt[function] =
          std::distance(function->begin(), function->end());
      func2InstrCnt[function] =
          std::distance(inst_begin(function), inst_end(function));
    }
  }
}; // end Profiler

struct StructCallMap {
  map<StructFieldPair *, set<GetElementPtrInst *> *> _structCallMap;
  Module *_module;
  StructCallMap(Module *m) { _module = m; }

  map<StructFieldPair *, set<GetElementPtrInst *> *>::iterator begin() {
    return _structCallMap.begin();
  }

  map<StructFieldPair *, set<GetElementPtrInst *> *>::iterator end() {
    return _structCallMap.end();
  }

  void insert(StructFieldPair *sfp, GetElementPtrInst *gepInst) {
    if (_structCallMap.count(sfp) == 0) {
      _structCallMap[sfp] = new set<GetElementPtrInst *>();
    }
    _structCallMap[sfp]->insert(gepInst);
  }

  void initStructCallMap(list<StructFieldPair *> sfps) {
    LOGINFO("Initializing struct call graph!\n");
    // collect all
    for (auto I = _module->begin(), E = _module->end(); I != E; ++I) {
      Function *F = &*I;
      for (auto iter = inst_begin(F); iter != inst_end(F); ++iter) {
        if (isa<GetElementPtrInst>(&*iter)) {
          GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(&*iter);
          if (gepInst->getNumOperands() != 3) {
            LOGERR("GEP's parameters are not 3!\n");
            continue;
          }

          StructFieldPair *sfp = gepMatchList(gepInst, sfps);
          if (sfp != nullptr) {
            insert(sfp, gepInst);
          }
        }
      }
    }
  }

  void initStructCallMapWithAll(map<StructFieldPair *, ResultCheckFunc *> mps) {
    LOGINFO("Initializing struct call graph!\n");
    // collect all
    for (auto I = _module->begin(), E = _module->end(); I != E; ++I) {
      Function *F = &*I;
      for (auto iter = inst_begin(F); iter != inst_end(F); ++iter) {
        if (isa<GetElementPtrInst>(&*iter)) {
          GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(&*iter);
          if (gepInst->getNumOperands() != 3) {
            LOGERR("GEP's parameters are not 3!\n");
            continue;
          }

          StructFieldPair *sfp = gepMatchFromMap(gepInst, mps);
          if (sfp != nullptr) {
            insert(sfp, gepInst);
          }
        }
      }
    }
  }

  set<GetElementPtrInst *> *getStructCallSites(StructFieldPair *sfp) {
    // StructFieldPair* sfp = gepMatchList(gepInst, sfps);
    return _structCallMap[sfp];
  }
};

struct ACFuncAnalyzer {
  Module *_module;
  // whole call graph of the .bc file
  MiniCallGraph *m_callGraph;

  // the map from the
  StructCallMap *m_structCallMap;

  // profile the number of instructions for all functions
  FunctionProfiler *m_profiler;

  // the exit functions, read from file, different for each .bc
  set<Function *> m_exitFuncs;
  set<string> m_libFuncs;

  // cache the taint analyis results for functions
  map<Function *, TaintAnalysis *> funcTaintAnalysisCache;

  // cache LogicAnalysis results
  map<Function *, LogicAnalysis *> funcLogicAnalysisCache;

  // This is the default analysis type, only
  // reserve this when we do the analysis, will be set to the best
  // value when we do the experiment
  enum ACFuncAnalysisType currAnalysisType = CNT_FUNCCALL_INSTRUCTION;

  // Flag to enable the PDT analysis
  // bug, the current ones does not consider exit functions, so
  // the false branch may dominate, causing results to be 0
  bool enablePDT;

  Module *_cloneModule;

  ACFuncAnalyzer(Module *M, list<string> exitFuncNames,
                 list<string> libFuncNames) {

    _module = &*M;
    _cloneModule = CloneModule(*_module).release();

    // m_callGraph = new MiniCallGraph(M);
    m_callGraph = g_callGraph;
    m_structCallMap = new StructCallMap(_module);

    // comment this, since we no longer need the function profiler
#ifdef AC_FUNC_ANALYZER_DEBUG
    m_profiler = new FunctionProfiler(*M);

    // print instr count in each function in m_profiler
    auto a = &(m_profiler->func2InstrCnt);
    auto iter = a->begin(), end = a->end();
    for (; iter != end; iter++) {
      errs() << iter->first->getName() << "\t" << iter->second << "\n";
    }
#endif

    enablePDT = false;

    //  init special functions
    setFuncs(exitFuncNames, &m_exitFuncs);
    setFuncNames(libFuncNames, &m_libFuncs);
    LOGINFO("initial log funcs\n");
    // printSet(&m_libFuncs, "\n");

    m_callGraph->printGraph();
  }

  bool writeCloneModule(string software) {
    // compose name if not given
    std::string fl;
    // if (!options.outputFile.empty()) {
    //     fl = options.outputFile;
    // } else {
    //     fl = options.inputFile;
    //     replace_suffix(fl, ".sliced");
    // }

    fl = software + "_log.bc";

    // open stream to write to
    std::ofstream ofs(fl);
    llvm::raw_os_ostream ostream(ofs);

    // write the module
    errs() << "[llvm-slicer] saving sliced module to: " << fl.c_str() << "\n";

#if (LLVM_VERSION_MAJOR > 6)
    llvm::WriteBitcodeToFile(*_module, ostream);
#else
    llvm::WriteBitcodeToFile(_module, ostream);
#endif

    return true;
  }

  void testModule() {
    errs() << "Testing in ACFuncAnalyzer with module\n";
    errs() << "address " << _module << "\n";
    if (_module->getFunction("main") == nullptr) {
      errs() << "ERROR!!!\n";
    } else {
      errs() << "GOOD!!!\n";
    }
    errs() << "address " << _module << "\n";
  }

  ~ACFuncAnalyzer() {
    delete m_callGraph;
#ifdef AC_FUNC_ANALYZER_DEBUG
    delete m_profiler;
#endif
  }

  void setACFuncAnalysisType(enum ACFuncAnalysisType type) {
    this->currAnalysisType = type;
  }

  void setFuncNames(list<string> funcNames, set<string> *funcSet) {
    if (funcNames.size() == 0)
      return;
    for (auto &it : funcNames) {
      funcSet->insert(it);
    }
  }
  void setFuncs(list<string> funcNames, set<Function *> *funcSet) {
    if (funcNames.size() == 0)
      return;

    for (auto &it : funcNames) {
      // we can get the func ptr with cached results in call graph
      Function *func = m_callGraph->getFuncPtrByDemangledName(it);
      LOGDEBUG("successfully get func by demangled name\n");
      if (func == nullptr) {
        LOGERR("can not find this function ", it, "!\n");
        assert(false);
      }
      funcSet->insert(func);
    }
  }

  void analyzeStructFuncPairs(list<StructFieldPair *> sfps) {
    // init the struct call map, find where this sftp is used
    // only init it when we have structFuncPairs()
    if (sfps.size() == 0) {
      LOGINFO("No struct field pairs for analysis.\n");
      return;
    }
    m_structCallMap->initStructCallMap(sfps);
    // for each call sit    e, ini the analysis by find the call site, it may
    // not be invoked as a call statements

    for (auto iter = m_structCallMap->begin(); iter != m_structCallMap->end();
         iter++) {
      for (auto instIter = iter->second->begin();
           instIter != iter->second->end(); ++instIter) {
        // for sfp's all used sites, perform analysis
        analyzeStructFuncPair(iter->first, *instIter);
      }
    }
  }

  void init_m_structCallMap(map<StructFieldPair *, ResultCheckFunc *> mps) {
    if (mps.size() > 0)
      m_structCallMap->initStructCallMapWithAll(mps);
  }
  void analyzeStructFuncPair(StructFieldPair *sfp, GetElementPtrInst *gepInst) {
    // first get the call inst first affected by the gepInst

    Function *func = gepInst->getParent()->getParent();
    TaintAnalysis *ans;
    if (funcTaintAnalysisCache.count(func) != 0) {
      LOGINFO("Got previous cached function taint analysis.\n");
      ans = funcTaintAnalysisCache[func];
    } else {
      TaintInfo mBottom;       // empty
      TaintInfo mInitialState; // empty
      funcTaintAnalysisCache[func] = new TaintAnalysis(mBottom, mInitialState);
      funcTaintAnalysisCache[func]->runWorklistAlgorithm(func);
      funcTaintAnalysisCache[func]->freeAllInfoButLastInfo();

      ans = funcTaintAnalysisCache[func];
    }

    set<Instruction *> affectedInstrs;
    ans->getImpactedCallInstr(gepInst, &affectedInstrs);
    // then use the first call instr to get the first affected branch basic
    // blocks
    if (affectedInstrs.size() == 0) {
      return;
    }

    Instruction *firstImpactedCallInst = *(affectedInstrs.begin());

    BasicBlock *parentBB = gepInst->getParent();
    set<BasicBlock *> visited;
    firstImpactedCallInst =
        getNextCallInstBFS(parentBB, &affectedInstrs, &visited);

    if (firstImpactedCallInst == nullptr) {
      LOGERR("Can't find the next call instr after original inst, next!\n");
      return;
    }
    LOGINFO("Affected call instruction", *firstImpactedCallInst, "\n");

    // we actually care about the call instruction!

    affectedInstrs.clear();
    ans->getImpactedBranchInstr(firstImpactedCallInst, &affectedInstrs);
    visited.clear();

    Instruction *firstImpactedBranchInst;
    firstImpactedBranchInst = getNextBranchInstBFS(
        firstImpactedCallInst->getParent(), &affectedInstrs, &visited);
    if (firstImpactedBranchInst == nullptr) {
      LOGERR("Can't find the next branch instr after call inst, next!\n");
      return;
    }

    analyzeBasicBlock(firstImpactedBranchInst->getParent(), m_profiler,
                      nullptr);
    std::cout << "[tmp] done with analysis\n";
  }

  // true means match with access denied value
  // false means not matched
  bool matchValue(int64_t value, unsigned bitlen, ResultCheckFunc *rcf) {
    /*  4399 is nullptr (constant pointer)
        -1000 is false (boolean)
        -2000 is true (boolean)
    */
    if (value == 0xffff) // no comparison value was set
      return false;
    if (bitlen == 1 && rcf->isBoolean()) { // boolean
      if (((rcf->denyVal == -1000 && value == 0) ||
           (rcf->denyVal == -2000 && value == 1)) &&
          (rcf->type == RESULT_EQ)) {
        return true;
      } else if (((rcf->denyVal == -1000 && value == 1) ||
                  (rcf->denyVal == -2000 && value == 0)) &&
                 (rcf->type == RESULT_NE)) {
        return true;
      } else
        return false;
    } else if (bitlen == 0 && value == NULLPTR_VAL &&
               rcf->isEquiv()) { // nullptr
      if ((rcf->denyVal == NULLPTR_VAL && value == NULLPTR_VAL) &&
          (rcf->type == RESULT_EQ)) {
        return true;
      } else if ((rcf->denyVal == NULLPTR_VAL && value == NULLPTR_VAL) &&
                 (rcf->type == RESULT_NE)) {
        return false;
      } else {
        LOGERR("Should not happen for the nullptr case!\n");
        return false;
      }
    } else if (bitlen == 32 && rcf->isEquiv()) {
      if ((rcf->denyVal == value) && (rcf->type == RESULT_EQ)) {
        return true;
      } else if ((rcf->denyVal == value) && (rcf->type == RESULT_NE)) {
        return false;
      } else if ((rcf->denyVal != value) && (rcf->type == RESULT_NE)) {
        return true;
      } else if ((rcf->denyVal != value) && (rcf->type == RESULT_EQ)) {
        return false;
      }
    } else if (bitlen == 32 &&
               rcf->type == RESULT_LT) { // This find the deny path, where the
                                         // return value  is
      if (value < rcf->denyVal)
        return true;
      if (value >= rcf->denyVal)
        return false;
    } else if (bitlen == 32 &&
               rcf->type == RESULT_GT) { // This find the deny path, where the
                                         // return value  is
      if (value > rcf->denyVal)
        return true;
      if (value <= rcf->denyVal)
        return false;
    } else if (bitlen == 32 &&
               rcf->type == RESULT_LE) { // This find the deny path, where the
                                         // return value  is
      if (value <= rcf->denyVal)
        return true;
      if (value > rcf->denyVal)
        return false;
    } else if (bitlen == 32 &&
               rcf->type == RESULT_GE) { // This find the deny path, where the
                                         // return value  is
      if (value >= rcf->denyVal)
        return true;
      if (value < rcf->denyVal)
        return false;
    } else {
      // NOT handled!!!
      LOGERR("NOT handled value match\n");
      return false;
    }
    return false;
  }

  bool findLogInAccessCheckFunction(Function *acfunc, ResultCheckFunc *rcf,
                                    Instruction **logInstr) {
    // TODO: How to know for sure there is a log in the function, and matches
    // RCF;
    if (rcf == nullptr)
      return false;
    // we start with match the most simple case, find store X, if in the same
    // block, track backwards until only has the unique successor.
    /* e.g. search for the instruction, store i1 true, i1* %2
    6:                                                ; preds = %1
    call void @_Z12my_log_errorv()
    store i1 true, i1* %2, align 1
    br label %8
    7:                                                ; preds = %1
    store i1 false, i1* %2, align 1
    br label %8
    8:                                                ; preds = %7, %6
    %9 = load i1, i1* %2, align 1
    ret i1 %9
    */
    bool hasLog = false;
    ReturnInst *retInst = nullptr;
    int numOfRet = 0;
    for (auto bb_itr = acfunc->begin(); bb_itr != acfunc->end(); ++bb_itr) {
      if (hasLogFunc(m_libFuncs, &*bb_itr, logInstr)) {
        hasLog = true;
      }
      for (auto itr = bb_itr->begin(); itr != bb_itr->end(); ++itr) {
        if (isa<ReturnInst>(&*itr)) {
          numOfRet += 1;
          retInst = dyn_cast<ReturnInst>(&*itr);
        }
      }
    }
    if (hasLog == false) {
      return false;
    }
    if (numOfRet >= 2) {
      LOGERR("More than 2 rets in the AC function\n");
      assert(false);
    }
    if (numOfRet == 0) {
      LOGERR("No rets in the AC function\n");
      assert(false);
    }
    int64_t contVal = 0xffff;
    unsigned bitlen = 0; // means defaultvalue

    Value *retValue = retInst->getReturnValue();
    Instruction *deniedBlockInstr = nullptr;

    // search log from return instr
    for (Value::use_iterator itr = retValue->use_begin();
         itr != retValue->use_end(); ++itr) {
      if (isa<Instruction>(*itr)) {
        Instruction *inst = dyn_cast<Instruction>(*itr);

        if (isa<LoadInst>(inst)) {
          LOGDEBUG("Find the load inst,", *inst, "\n");

          LoadInst *loadInst = dyn_cast<LoadInst>(inst);

          Value *src = loadInst->getOperand(0);
          LOGDEBUG("Find operand,", *src, "\t", *src->getType(), "\n");
          // search for all uses of %2, focus on store inst

          for (llvm::User *litr : src->users()) {
            if (isa<Instruction>(litr)) {
              Instruction *tmp = dyn_cast<Instruction>(litr);
              LOGDEBUG("Find users of operand,", *tmp, "\n");
              if (isa<StoreInst>(tmp)) {
                Value *src = tmp->getOperand(0);
                // Value *tgt = tmp->getOperand(1);
                if (isa<ConstantInt>(src)) {
                  contVal = dyn_cast<ConstantInt>(src)->getSExtValue();
                  bitlen = dyn_cast<ConstantInt>(src)->getBitWidth();
                  LOGDEBUG("Find store val,", contVal, " with ", bitlen,
                           "bits \n");
                } else if (isa<ConstantPointerNull>(src)) {
                  contVal = NULLPTR_VAL;
                  LOGDEBUG("Find store val,", contVal, "\n");
                }

                if (matchValue(contVal, bitlen, rcf)) {
                  // when matches the denied branch, set it!
                  // TODO may be multiple places to set it
                  deniedBlockInstr = tmp;
                  bitlen = 0; // reset bitlen
                }
              }
            }
          }
        } else {
          LOGERR("Don't support other inst for now\n");
        }
      }
    }

    // only search for log that is just before the deny
    if (deniedBlockInstr != nullptr) {
      // TODO need backwards tracking along the unique predecessor!
      // return hasLogFunc(m_libFuncs, deniedBlockInstr->getParent(),logInstr);
      auto loopIter = deniedBlockInstr->getParent();
      while (loopIter != nullptr) {
        LOGDEBUG("Searching for Log function\n");
        if (hasLogFunc(m_libFuncs, loopIter, logInstr)) {
          return true;
        }
        loopIter = loopIter->getUniquePredecessor();
        if (loopIter == nullptr) {
          // postdominator but may have more than 1 predecessor
          break;
        }
        // the previous block should also has only one successor
        if (loopIter->getUniqueSuccessor() == nullptr) {
          break;
        }
      }
    }
    return false;
  }

  bool hasLogInDeniedBranch(set<string> logFuncs, Instruction *tbInstr,
                            int deBranch, Instruction **logInstr) {
    BranchInst *inst = dyn_cast<BranchInst>(tbInstr);
    assert(deBranch != -1);
    /// 1. from the denied branch to end, no other branches
    BasicBlock *startBB = inst->getSuccessor(deBranch);
    errs() << *startBB << "\n";

    BasicBlock *loopIter = startBB;
    bool hasLog1 = false;
    while (loopIter != nullptr) {
      LOGDEBUG("Searching for Log function\n");
      if (hasLogFunc(logFuncs, loopIter, logInstr)) {
        // TODO: should check if this block does not post dominate both
        //       true branch and false branch
        LOGINFO("Found log function!!!!!", **logInstr, "\n");
        hasLog1 = true;
        break;
      }
      // loopIter will be nullptr if has 2 or more successors
      loopIter = loopIter->getUniqueSuccessor();
    }
    return hasLog1;
    // if not found, see if the error is propagated to the caller via
    // return value, more specifically, if %ret is tained by original

    // we only check this when NO previous log exists.
    // the denied branch simply returns the value,
    // then check in the upper level, if the denied value is checked or not
  }

  DeniedBranchContainer *findLogAtUpperLevel(Instruction *instr,
                                             ResultCheckFunc *rcf) {
    LOGERR("Searching for log at upper level!!!\n");
    DeniedBranchContainer *resultContainer = nullptr;

    Function *func = instr->getParent()->getParent();
    // the logic analysis, first do the taint analysis,
    // then construct the logic relation tree based on the analysis result.
    LogicAnalysis *ans;

    if (funcLogicAnalysisCache.count(func) != 0) {
      LOGINFO("Got previous cached function logic analysis.\n");
      ans = funcLogicAnalysisCache[func];
    } else {
      LogicInfo mBottom;       // empty
      LogicInfo mInitialState; // empty
      funcLogicAnalysisCache[func] = new LogicAnalysis(mBottom, mInitialState);
      funcLogicAnalysisCache[func]->runWorklistAlgorithm(func);
      ans = funcLogicAnalysisCache[func];
    }

    set<Instruction *> affectedInstrs;
    // now we get all conditional branches
    // ans->getImpactedBranchInstr(instr, &affectedInstrs);

    if (affectedInstrs.size() == 0) {
      LOGERR("THIS IS RARE, no affected branch instructions, next!\n");

      // This is to deal with the case that the value is simply returned without
      // check!
      bool isRetImpacted = ans->isRetImpactedByInstr(instr);
      // if track at upper level, the rcf may already changed? thus maybe
      // meaningless to track upwards.

      if (isRetImpacted) {
        // continuously tracking to all functions that used this
        // in the call graph
        Function *func = instr->getFunction();
        MiniGraphNode *node = m_callGraph->getCallGraphNode(func);
        if (node == nullptr) {
          LOGERR("Could not find the callsites of the function ",
                 func->getName(), "\n");
        }
        LOGINFO(func->getName(), " is called at ", node->_callSite.size(),
                " places\n");

        if (node->_callSite.size() != 1)
          return nullptr;

        for (Instruction *itr : node->_callSite) {
          return findLogAtUpperLevel(itr, rcf);
        }
      }
      return nullptr;
    }

    // how to know if rcf works on the upper level - assume it
    int cntt = 0;
    for (auto brInstr : affectedInstrs) {
      cntt++;
      // std::cout << func->getName().str() << "\t with affected BR " << cntt
      // <<"\n"; LOGINFO(*instr, "\t with affected BR ", cntt, "\t",
      // *brInstr,"\n");
      int deniedBranch = ans->searchDeniedBranch(instr, brInstr, rcf);
      //
      if (deniedBranch != -1) {
        ////// Case 1: If the log exists after denied branch?

        if (resultContainer != nullptr) {
          LOGERR("This instruction already has one impacted branch instr!!!\n");
          break;
        }
        resultContainer =
            new DeniedBranchContainer(brInstr, deniedBranch, instr);

        Instruction *logInstr = nullptr;
        bool hasLog1 =
            hasLogInDeniedBranch(m_libFuncs, brInstr, deniedBranch, &logInstr);
        LOGINFO("Did we find logs at upper level?", hasLog1 ? "true" : "false",
                "\n");
        if (hasLog1 && logInstr != nullptr) {
          resultContainer->setLogLocation(hasLog1, &logInstr);
        }

        if (!hasLog1) {
          bool isRetImpacted = ans->isRetImpactedByInstr(instr);
          DeniedBranchContainer *upperContainer;
          if (isRetImpacted) {
            // continuously tracking to all functions that used this
            // in the call graph

            MiniGraphNode *node = m_callGraph->getCallGraphNode(func);
            if (node == nullptr) {
              LOGERR("Could not find the callsites of the function ",
                     func->getName(), "\n");
            }
            LOGINFO(func->getName(), " is called at ", node->_callSite.size(),
                    " places\n");

            if (node->_callSite.size() == 1) {
              for (Instruction *itr : node->_callSite) {
                // if the ac func does not have log at the call site;
                // we keep tracking upwards
                upperContainer = findLogAtUpperLevel(itr, rcf);
                if (upperContainer != nullptr) {
                  resultContainer->setLogLocation(upperContainer->hasLog,
                                                  &(upperContainer->logInstr));
                  resultContainer->setIsLogAtUpperLevel(true);
                }
              }
            }
          }
        }
      }
    }
    return resultContainer;
  }

  // This one do the actual work :)
  DeniedBranchContainer *findDeniedBranch(Instruction *instr,
                                          ResultCheckFunc *rcf) {
    DeniedBranchContainer *resultContainer = nullptr;

    Function *func = instr->getParent()->getParent();
    // the logic analysis, first do the taint analysis,
    // then construct the logic relation tree based on the analysis result.
    // LogicAnalysis * ans;
    // LogicInfo mBottom;
    // LogicInfo mInitiateState;
    // ans = new LogicAnalysis(mBottom, mInitiateState);
    // ans->runWorklistAlgorithm(func);
    LogicAnalysis *ans;
    if (funcLogicAnalysisCache.count(func) != 0) {
      LOGINFO("Got previous cached function logic analysis.\n");
      ans = funcLogicAnalysisCache[func];
    } else {
      LogicInfo mBottom;       // empty
      LogicInfo mInitialState; // empty
      funcLogicAnalysisCache[func] = new LogicAnalysis(mBottom, mInitialState);
      funcLogicAnalysisCache[func]->runWorklistAlgorithm(func);
      ans = funcLogicAnalysisCache[func];
    }

    // 3.0, get all branch instructions that has been impacted by original instr
    set<Instruction *> affectedInstrs;
    // now we get all conditional branches
    ans->getImpactedBranchInstr(instr, &affectedInstrs);

    if (affectedInstrs.size() == 0) {
      LOGERR("THIS IS RARE, no affected branch instructions, next!\n");

      // This is to deal with the case that the value is simply returned without
      // check!
      bool isRetImpacted = ans->isRetImpactedByInstr(instr);
      // if track at upper level, the rcf may already changed? thus maybe
      // meaningless to track upwards.

      if (isRetImpacted) {
        // continuously tracking to all functions that used this
        // in the call graph
        Function *func = instr->getFunction();
        MiniGraphNode *node = m_callGraph->getCallGraphNode(func);
        if (node == nullptr) {
          LOGERR("Could not find the callsites of the function ",
                 func->getName(), "\n");
        }
        LOGINFO(func->getName(), " is called at ", node->_callSite.size(),
                " places\n");

        if (node->_callSite.size() != 1)
          return nullptr;

        for (Instruction *itr : node->_callSite) {
          return findLogAtUpperLevel(itr, rcf);
        }
      }
      return nullptr;
    }

    // two hopes
    // (1) first we get the denied branch with the value comparison
    // (2) the return value of this function, is the value of check function
    // result, then track upwards

    // from the branch instr, back track to the call instr
    int cntt = 0;
    for (auto brInstr : affectedInstrs) {
      cntt++;
      std::cout << func->getName().str() << "\t with affected BR " << cntt
                << "\n";
      LOGINFO(*instr, "\t with affected BR ", cntt, "\t", *brInstr, "\n");
      int deniedBranch = ans->searchDeniedBranch(instr, brInstr, rcf);
      //
      if (deniedBranch != -1) {
        ////// Case 1: If the log exists after denied branch?

        if (resultContainer != nullptr) {
          LOGERR("This instruction already has one impacted branch instr!!!\n");
          break;
        }
        resultContainer =
            new DeniedBranchContainer(brInstr, deniedBranch, instr);

        Instruction *logInstr = nullptr;
        bool hasLog1 =
            hasLogInDeniedBranch(m_libFuncs, brInstr, deniedBranch, &logInstr);
        LOGINFO("Did we find logs after access check function?",
                hasLog1 ? "true" : "false", "\n");
        if (hasLog1) {
          LOGDEBUG(*logInstr, "\n");
          resultContainer->setLogLocation(hasLog1, &logInstr);
          LOGDEBUG(*(resultContainer->getLogInstruction()), "\n");
        }

        if (!hasLog1) {
          ////// Case 2: See if log in the upper level
          //          (1) first need to make sure that the ret is affected by
          //          instr (2) start tracking upwards, if the return value is
          //          checked
          //              we need to make sure that the return value is exactly
          //              the same one , so we can reuse the result check
          //              function in the upper level
          bool isRetImpacted = ans->isRetImpactedByInstr(instr);
          DeniedBranchContainer *upperContainer;
          if (isRetImpacted) {
            // continuously tracking to all functions that used this
            // in the call graph

            MiniGraphNode *node = m_callGraph->getCallGraphNode(func);
            if (node == nullptr) {
              LOGERR("Could not find the callsites of the function ",
                     func->getName(), "\n");
            }
            LOGINFO(func->getName(), " is called at ", node->_callSite.size(),
                    " places\n");

            // only track upwards, when only one callsite, if multiple callsite,
            // the log is not specific.
            if (node->_callSite.size() == 1) {

              for (Instruction *itr : node->_callSite) {
                // if the ac func does not have log at the call site;
                // we keep tracking upwards

                upperContainer = findLogAtUpperLevel(itr, rcf);
                if (upperContainer != nullptr) {
                  // ge
                  resultContainer->setLogLocation(upperContainer->hasLog,
                                                  &(upperContainer->logInstr));
                  resultContainer->setIsLogAtUpperLevel(true);
                }
              }
            }
          }
        }
      }
    }

    return resultContainer;
  }

  // get relevant parameters for the access check function
  void getInsertParameters(
      Module *m, string acFuncName, ResultCheckFunc *rcf,
      set<ParamPos> &slicedParams, // relevant AC func parameters
      // map<int, set<int>* >& slicedParamField, // if AC func parameter, also
      // get which field is relevant
      set<Value *> &gvInACFunc // relevant global variables in AC func
  ) {
    set<Instruction *> res; // deny points
    // set<Value*> slicedParams;
    // 1.1 get deny points inside the RCF
    getDenyReturnPoints(acFuncName, rcf, res);

    if (res.size()) {
      printSetInstr(&res, "\t");
      LOGERR("\n============\n");
      // 1.2 get relevant parameters from the check function
      getSlicedParametersFromDependeceGraph(m, acFuncName, rcf, res,
                                            slicedParams, gvInACFunc);
    } else {
      LOGERR(
          "\n cannot find any return values in the access-check function.\n");
      LOGERR("\n No parameters from the access-check function are sliced.\n");
    }
  }

  void getLocalVaraiablesAtCallSite(
      Module *m,
      Function *func,             // callsite's function name
      Instruction *instr,         // call instruction
      set<ParamPos> slicedParams, // index of parameters
      set<Value *> &allSlicedVarValues,
      set<ParamPos> &allSlicedArgValues) // store the results of backward
                                         // slicing :) at the caller
  {
    CallInst *callInst = dyn_cast<CallInst>(instr);
    if (callInst == nullptr)
      LOGERR("call site is nullptr!!!\n");
    for (auto &idx : slicedParams) {
      if ((unsigned)idx.first >= callInst->getNumArgOperands()) {
        LOGERR("call site has invalid sliced argument index!!!\n");
        continue;
      }

      Value *loadValue = callInst->getArgOperand((unsigned)idx.first); //
      if (loadValue) { // if this value is a struct, we also track all fields of
                       // this struct
        if (loadValue->getType()->isPointerTy()) {

          Type *pointeeTy = loadValue->getType()->getPointerElementType();
          if (pointeeTy->isStructTy()) {
            // the following instruction will be here;
            // %11 = load %struct.request*, %struct.request** %a, align 8, !dbg
            // !34 we need to track each field ! = =
          }
        }
      }

      if (Instruction *slicePoint = dyn_cast<Instruction>(loadValue)) {
        set<Value *> sliceResult; // GVs
        set<ParamPos> sliceArgs;
        getSlicedParametersOrGlobalVars(m, func, slicePoint, sliceResult,
                                        sliceArgs);
        allSlicedVarValues.insert(sliceResult.begin(), sliceResult.end());
        allSlicedArgValues.insert(sliceArgs.begin(), sliceArgs.end());
      } else if (isa<GlobalVariable>(loadValue)) {
        // the arg which is feed into the access check function, is a global
        // variable
        allSlicedVarValues.insert(loadValue);
      } else if (isa<ConstantExpr>(loadValue)) {
        ConstantExpr *CE = dyn_cast<ConstantExpr>(loadValue);

        for (unsigned i = 0, e = CE->getNumOperands(); i != e; ++i) {
          if (GlobalVariable *srcOp =
                  dyn_cast<GlobalVariable>(CE->getOperand(i))) {
            allSlicedVarValues.insert(srcOp);
          }
        }
      }
    }
  }

  void deniedBranchLookUpByName(string funcName, ResultCheckFunc *rcf) {
    MiniGraphNode *graphNode = m_callGraph->getCallGraphNodeByName(funcName);
    // debug
    if (graphNode == nullptr) {
      LOGERR("Sorry, can not find this function: ", funcName, "\n");
      return;
    }

    int lcnt = 0;
    // (0) In the access check function; (checked in the first place )
    Instruction *logInstrInACFunc = nullptr;
    bool hasLogInACFunc = findLogInAccessCheckFunction(graphNode->_function,
                                                       rcf, &logInstrInACFunc);
    LOGINFO("Did we find logs inside the AC func? ",
            hasLogInACFunc ? "true" : "false", funcName, "\n");

    // (0.1) Get relevant parameters;
    set<ParamPos> slicedParams; // should only record position here;
    set<Value *> slicedGV;      // relevant GV in AC func
    getInsertParameters(_module, funcName, rcf, slicedParams, slicedGV);
    // (0.2) Get more relevant values;

    // if the ac func is a function pointer, only
    // see if it contains log inside
    if (graphNode->_callSite.size() == 0) {
      // funcname, usage #, has log in AC?, in function, denied branch,
      errs() << "[summary]" << funcName << "," << -1 << ","
             << ""
             << "," << -1 << "," << (hasLogInACFunc ? 1 : 0)
             << ","       // has log in AC or not
             << -1 << "," // has log at call site?
             << -1 << "," // has log at call site?
             << slicedParams.size() << "|" << slicedGV.size() << ","
             << ""
             << ","
             << ""
             << ","
             << ""
             << "\n";
      // delete ans;
    }

    // for the lib calls, like apr_file_open(),
    // we can not perform analysis or get relevant parameters for AC func
    if (slicedParams.size() == 0) {
      errs() << "LIBCALL " << funcName << "with "
             << graphNode->_function->size() << " blocks"
             << "\n";
      // in this case, we think all parameters as relevant
    }

    LOGERR("Sliced arg result\n");
    printSetPairs(&slicedParams, "\n");

    for (auto instr : graphNode->_callSite) {
      errs() << "\n"; // keep this to separate for differnet call sites
      lcnt++;
      std::cout << funcName << ":" << lcnt << endl;
      LOGINFO("===================\n", funcName, " Usage #", lcnt,
              "\tby: ", *instr, "\n");

      // 1.0 start from the function
      Function *func = instr->getParent()->getParent();
      LOGINFO(funcName, " Usage in ", func->getName(), "\n");

      // 2.0 get relevant values for logging
      //      from the position at the function arguments, to
      //      get the real variables at the caller
      map<Value *, pair<Value *, int>>
          slicedParamValues; // <originalArg, <allocaArg, field> >
      // set<Value*> slicedParamValues;
      getSlicedCallArgs(instr, slicedParams, slicedParamValues);
      // get critical information at call site
      // mainly the data dependencies, see, if can track to
      // live-in variables (global vars and func parameters)

      // 3.0 local information through dependency analysis
      set<Value *> localVars; // GVs at the callsite func
      set<ParamPos> localFuncArgs;
      getLocalVaraiablesAtCallSite(_module, func, instr, slicedParams,
                                   localVars, localFuncArgs);

      // merge GVs at caller and callee
      localVars.insert(slicedGV.begin(), slicedGV.end());

      DeniedBranchContainer *denyBranchContainer = findDeniedBranch(instr, rcf);
      string insertLogFormat = "";

      // print the log for results collection
      insertLogFormat = getInsertLogOnDenial(
          _module, func, instr,
          nullptr,           // denied branch container :)
          slicedParamValues, // sliced variable from acc function
          localVars,         // global variables from both ACC and call site
          localFuncArgs);    // parameters of the call site function

      if (denyBranchContainer != nullptr) {
        // with the denied branch, we should insert log here!
        LOGERR("WE FOUND THE DENIED BRANCH!\n");
        // first, just insert a call of the log function in the denied block :)
        insertLogFormat =
            getInsertLogOnDenial(_module, func, instr, denyBranchContainer,
                                 slicedParamValues, localVars, localFuncArgs);
        // denyBranchContainer->print();
      } else {
        // TODO: insert error checking code and the log messages
        LOGERR("DENIED BRANCH NOT FOUND!\n");
        insertLogFormat = getInsertLogOnDenial(_module, func, instr, nullptr,
                                               slicedParamValues, localVars,
                                               localFuncArgs, true, rcf);
      }

      // funcname, usage #, has log in AC?, in function, denied branch,
      errs() << "[summary]" << funcName << "," << lcnt << "," << func->getName()
             << ","
             << to_string(denyBranchContainer
                              ? denyBranchContainer->deniedBranch
                              : -1)
             << "," << (hasLogInACFunc ? 1 : 0) << "," // has log in AC or not
             << to_string(denyBranchContainer ? denyBranchContainer->hasLog
                                              : -1)
             << "," // has log at call site?
             << to_string(denyBranchContainer
                              ? denyBranchContainer->isLogAtUpperLevel
                              : -1)
             << "," // has log at call site?
             << slicedParamValues.size() << "," << localVars.size() << ","
             << localFuncArgs.size() << "," << insertLogFormat << "\n";

      std::cout << "[tmp] done with denied branch identification analysis\n";
      LOGINFO("================DONE\n", funcName, " Usage #", lcnt,
              "\tby: ", *instr, "\n");
      // delete ans;
    }
  }

  void deniedBranchLookUpByStructFieldMap(StructFieldPair *sfp,
                                          ResultCheckFunc *rcf) {
    set<GetElementPtrInst *> *callsites =
        m_structCallMap->getStructCallSites(sfp);
    // debug
    if (callsites == nullptr) {
      LOGERR("Sorry, can not find this function: ", sfp->sname, "\n");
      return;
    }
    // for each struct function pointer, may be used at many places

    if (callsites->size() == 0) {
      LOGERR("This struct function has no call sites: ", sfp->sname, "\n");
      return;
    }

    LOGERR("[Struct function] ", sfp->sname, "has been used at ",
           callsites->size(), " places\n");

    int loopCnt = 0;
    for (auto &gepInst : *callsites) {
      Function *func = gepInst->getParent()->getParent();

      LOGERR("[Struct function] call site ", loopCnt, " at ", *gepInst);

      LogicAnalysis *ans;
      if (funcLogicAnalysisCache.count(func) != 0) {
        LOGINFO("Got previous cached function logic analysis.\n");
        ans = funcLogicAnalysisCache[func];
      } else {
        LogicInfo mBottom;       // empty
        LogicInfo mInitialState; // empty
        funcLogicAnalysisCache[func] =
            new LogicAnalysis(mBottom, mInitialState);
        funcLogicAnalysisCache[func]->runWorklistAlgorithm(func);
        ans = funcLogicAnalysisCache[func];
      }

      set<Instruction *> affectedInstrs;
      ans->getImpactedCallInstr(gepInst, &affectedInstrs);
      // then use the first call instr to get the first affected branch basic
      // blocks
      if (affectedInstrs.size() == 0) {
        LOGERR("[Struct function] No impacted call instrs\n");
        continue; // just skip to next call site
      }

      Instruction *firstImpactedCallInst = *(affectedInstrs.begin());

      BasicBlock *parentBB = gepInst->getParent();
      set<BasicBlock *> visited;
      firstImpactedCallInst =
          getNextCallInstBFS(parentBB, &affectedInstrs, &visited);

      if (firstImpactedCallInst == nullptr) {
        LOGERR("[Struct function] Can't find the next call instr after "
               "original inst, next!\n");
        continue; // just skip to next call site
      }
      LOGINFO("[Struct function] First affected call instruction",
              *firstImpactedCallInst, "\n");

      // we actually care about the call instruction! firstImpactedCallInst
      affectedInstrs.clear();
      ans->getImpactedBranchInstr(firstImpactedCallInst, &affectedInstrs);
      visited.clear();

      // TODO: we will handle the denied branch later!!!!
      // Instruction* firstImpactedBranchInst;
      // firstImpactedBranchInst =
      // getNextBranchInstBFS(firstImpactedCallInst->getParent(),
      // &affectedInstrs, &visited); if (firstImpactedBranchInst == nullptr) {
      //     LOGERR("Can't find the next branch instr after call inst,
      //     next!\n"); return;
      // }
      // LOGINFO("[Struct function] First affected branch instruction",
      // *firstImpactedBranchInst, "\n");

      CallInst *callInst = dyn_cast<CallInst>(firstImpactedCallInst);
      // to learn about the call function, the callInst->getNumOperands()-1 is
      // the function

      // (0.1) All the parameters are relevant
      set<ParamPos> slicedParams; // should only record position here;
      set<Value *> slicedGV;      // relevant GV in AC func => we ignore here

      // This is only for arguments
      for (unsigned i = 0; i < callInst->getNumArgOperands(); ++i) {
        slicedParams.insert(make_pair(i, -1));
      }

      // 2.0 get relevant values for logging
      //      from the position at the function arguments, to
      //      get the real variables at the caller
      map<Value *, pair<Value *, int>>
          slicedParamValues; // <originalArg, <allocaArg, field> >
      // set<Value*> slicedParamValues;
      getSlicedCallArgs(callInst, slicedParams, slicedParamValues);

      // 3.0 local information through dependency analysis
      set<Value *> localVars; // GVs at the callsite func
      set<ParamPos> localFuncArgs;
      getLocalVaraiablesAtCallSite(_module, func, callInst, slicedParams,
                                   localVars, localFuncArgs);
      localVars.insert(slicedGV.begin(), slicedGV.end());

      // TODO: NEW parameters
      string insertLogFormat =
          getInsertLogOnDenial(_module, func, callInst, nullptr,
                               slicedParamValues, localVars, localFuncArgs);

      // funcname, usage #, has log in AC?, in function, denied branch,
      errs() << "[summary]" << sfp->sname << "," << loopCnt << ","
             << func->getName() << "," << to_string(-1) << "," << to_string(-1)
             << ","                  // has log in AC or not
             << to_string(-1) << "," // has log at call site?
             << to_string(-1) << "," // has log at call site?
             << slicedParamValues.size() << "," << localVars.size() << ","
             << localFuncArgs.size() << "," << insertLogFormat << "\n";
      std::cout << "[tmp] done with denied branch identification analysis\n";

      loopCnt += 1;
    }
  }

  /*
  targetVar is the current value we are looking for, that were affect the return
  value. linkedlist is a path of block that will lead to a deny return.
  seekStore is bool indicate whether we know this is a deny path yet.
  current means the current block that needs to be analyss
   */
  struct CFPath {
    Value *targetVar;
    list<BasicBlock *> linkedlist;
    bool seekStore;
    BasicBlock *current;
    set<Value *> affectArgs;
    set<Value *> affectTrimedArgs;
    CFPath(BasicBlock *c, bool s) {
      current = c;
      seekStore = s;
    }

    void addBlock(BasicBlock *c) { linkedlist.push_front(c); }
  };

  BranchInst *isBranchConditional(BasicBlock *B) {

    for (auto II = B->begin(), IE = B->end(); II != IE; ++II) {
      // Instruction *i = &*II;
      if (isa<BranchInst>(&*II)) {
        BranchInst *BInst = dyn_cast<BranchInst>(&*II);
        // errs() <<  *i <<" hum???\n";
        if (BInst->isConditional()) {
          // errs() <<   "--------this is conditional---\n";
          return BInst;
        }
      }
    }
    // errs() <<   "--------this is not conditional---\n";
    return NULL;
  }

  bool isBranchReturn(BasicBlock *BB) {
    bool ret = false;
    for (auto II = BB->begin(), IE = BB->end(); II != IE; ++II) {
      if (isa<ReturnInst>(&*II)) {
        ret = true;
      }
    }
    return ret;
  }

  bool isBranchAffectResult(BasicBlock *BB, BasicBlock *EB,
                            list<Instruction *> branchList, Value *retVal,
                            LogicAnalysis *ans) {
    // If B's child has effect on return value or pervious branch.
    // find the index of them
    // loop the index
    // get the value base on the index
    // each block just check.
    // auto A = &(B->getNext());
    // bb->

    bool result = false;
    // fisrt step, use BFS to find all the block
    list<BasicBlock *> fifoQueue;
    fifoQueue.push_back(BB);
    bool first = true;
    while (fifoQueue.size() > 0) {
      BasicBlock *currentBlock = fifoQueue.front();
      fifoQueue.pop_front();
      for (auto pi = pred_begin(currentBlock), pe = pred_end(currentBlock);
           pi != pe; ++pi) {
        BasicBlock *blk = *pi;
        if (blk != EB) {
          fifoQueue.push_back(blk);
        }
      }
      if (first) {
        first = false;
        continue;
      }
      // check if this block affect
      set<Instruction *> affectedInstrs;
      for (BasicBlock::iterator II = currentBlock->begin(),
                                IE = currentBlock->end();
           II != IE; ++II) {
        Instruction *i = &*II;
        ans->getImpactedInstr(i, &affectedInstrs, ALL_TYPE);
        for (auto ins : affectedInstrs) {
          for (auto branch : branchList) {
            if (ins == branch) {
              return true;
            }
          }
          if (ins == retVal) {
            return true;
          }
        }
      }
    }
    return result;
  }

  // TODO: if return vale not a constast need to know the impacted
  // Need to know if return value is a constant
  void printMod(Module *M) {
    errs() << "print module"
           << "\n";
    for (auto FB = M->begin(), FE = M->end(); FB != FE; ++FB) {
      for (auto BB = FB->begin(), BE = FB->end(); BB != BE; ++BB) {
        for (auto II = BB->begin(); II != BB->end(); ++II) {
          errs() << *II << "\n";
        }
      }
    }
  }

  // used at each AC function call site
  // we do it different by only tracking the data dependency graph
  void getSlicedParametersOrGlobalVars(Module *m, Function *func,
                                       Instruction *slicePoint,
                                       set<Value *> &depenedentVars,
                                       set<ParamPos> &dependentArgs) {
    DepGraph *fgraph = new DepGraph(m, func, true);
    fgraph->buildOnlyDataDepGraph();

    set<Value *> result = fgraph->sliceInstrs(slicePoint);

    for (auto I : result) {
      // e.g. load from a global variable
      if (isa<GlobalVariable>(I)) {
        depenedentVars.insert(I);
      } else {
        Instruction *inst = dyn_cast<Instruction>(I);
        for (unsigned i = 0; i < inst->getNumOperands(); i++) {
          if (GlobalVariable *G =
                  dyn_cast<GlobalVariable>(inst->getOperand(i))) {
            errs() << "Global value! " << *G << "\n";
            depenedentVars.insert(G);
          }
        }
      }
    }

    // see what is sliced in the arguments :)
    set<Value *> GV; // not used
    fgraph->getSlicedArgs(slicePoint, dependentArgs, GV);
  }

  // used for each access check function
  void getSlicedParametersFromDependeceGraph(Module *m, string funcName,
                                             ResultCheckFunc *rcf,
                                             set<Instruction *> &slicePoints,
                                             set<ParamPos> &params,
                                             set<Value *> &gvInACFunc) {
    Function *entry = g_callGraph->getCallGraphNodeByName(funcName)->_function;

    DepGraph *fgraph = new DepGraph(m, entry, true);
    fgraph->buildGraph();
    set<Value *> result;
    for (auto &point : slicePoints) {
      fgraph->getSlicedArgs(point, params, gvInACFunc);
    }
  }
  /*
    void checkFuncAnalysis(string funcName, ResultCheckFunc *rcf) {
      // Makes a copy of the Module
      auto start = std::chrono::high_resolution_clock::now();
      auto copyM = CloneModule(*_module).release();
      auto stop = std::chrono::high_resolution_clock::now();
      auto duration =
          std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
      errs() << "Time taken to clone a module" << duration.count()
             << "microseconds\n";

      // auto copyM = *_module;
      // MiniGraphNode* graphNode =
    m_callGraph->getCallGraphNodeByName(funcName);
      // Function *entry = graphNode->_function; // the entry is in _module; but
      // not copyM; if (!entry) {
      //     LOGERR("Sorry, can not find this function: ", funcName, "\n");
      //     return;
      // }

      Function *entry = copyM->getFunction(funcName);

      dg::llvmdg::LLVMDependenceGraphOptions dgOptions{};
      dgOptions.entryFunction = "main";
      ModuleWriter writer(funcName, copyM);
      // #ifdef RM_UNUSED
      // writer.removeUnusedFromModule();
      // #endif

      SlicerOptions options;
      options.dgOptions = dgOptions;
      MySlicer slicer(copyM, options);
      // build dependency graph
      if (!slicer.buildDG()) {
        errs() << "ERROR: Failed building DG\n";
        return;
      }
      // FIXME: slicingInstruction should be the store before each deny path.
    This
      // should be running outside of this funciton.
      std::set<llvm::Instruction *> slicingInstructions;
      int third = 0;

      for (llvm::Function::iterator BB = entry->begin(), BBE = entry->end();
           BB != BBE; ++BB) {
        for (llvm::BasicBlock::iterator II = BB->begin(), IE = BB->end();
             II != IE; ++II) {
          if (llvm::isa<llvm::StoreInst>(&*II)) {
            llvm::Instruction *I = &*II;
            third++;
            if (third == 3) {
              slicingInstructions.insert(I);
              break;
            }
          }
        }
      }
      for (auto I : slicingInstructions) {
        errs() << *I << "\n";
      }
      // for (auto I : slicingInstructions) {
      //     errs() << *(I->getParent()->getParent()) << "\n";
      // }

      set<LLVMNode *> criteria_nodes;
      slicer.getDG().getInstructionSites(slicingInstructions, &criteria_nodes);
      // --------make the criteria nodes mine.

      if (criteria_nodes.empty()) {
        llvm::errs() << "Did not find slicing criteria \n";
      }

      errs() << slicingInstructions.size() << " slcie instructions\n";
      errs() << criteria_nodes.size() << " criteria nodes\n";
      errs() << "Done with criteria!\n";
      // mark nodes that are going to be in the slice
      if (!slicer.mark(criteria_nodes)) {
        llvm::errs() << "Finding dependent nodes failed\n";
        return;
      }

      // slice the graph
      if (!slicer.slice()) {
        errs() << "ERROR: Slicing failed\n";
        return;
      }
      // slicer.sliceByFunctionName("vsf_access_check_number");
      // remove unneeded parts of the module
      writer.removeUnusedFromModule();
      //     // fix linkage of declared functions (if needs to be fixed)
      writer.makeDeclarationsExternal();

      // errs() << *(copyM->getFunction(funcName));
      writer.cleanAndSaveModule();
      return;
    }
  */
  void findArgs(Function *checkfunc, list<Value *> &args) {
    for (auto ab = checkfunc->arg_begin(), ae = checkfunc->arg_end(); ab != ae;
         ++ab) {
      auto abb = &*ab;
      Value *a = dyn_cast<Value>(abb);
      // Instruction * vv = (Instruction *) a;
      args.push_back(a);
    }
  }

  bool matchRCF(Instruction *inst, ResultCheckFunc *rcf) {
    Value *first = inst->getOperand(0);
    if (!isa<Constant>(first)) {
      // if the variable is stored to first; then we know this may be a denypath
      // TODO: constant propagation to infer if first is constant or not!!
      return true;
    }

    int64_t contVal = 0xffff;
    unsigned bitlen = 0; // means defaultvalue

    if (isa<ConstantInt>(first)) {
      contVal = dyn_cast<ConstantInt>(first)->getZExtValue();
      bitlen = dyn_cast<ConstantInt>(first)->getBitWidth();
      // LOGDEBUG("Find store val,", contVal," with ", bitlen, "bits \n");
    } else if (isa<ConstantPointerNull>(first)) {
      contVal = NULLPTR_VAL;
      LOGDEBUG("Find store val,", contVal, "\n");
    }

    return matchValue(contVal, bitlen, rcf);
  }

  // use the deny point to find backward slice the parameters
  //
  void getDenyReturnPoints(string funcName, ResultCheckFunc *rcf,
                           set<Instruction *> &res) {
    // actually are store instructions
    // or phi nodes
    MiniGraphNode *graphNode = m_callGraph->getCallGraphNodeByName(funcName);
    if (graphNode == nullptr) {
      LOGERR("Sorry, can not find this function: ", funcName, "\n");
      return;
    }
    // get the function
    Function *checkFunc = graphNode->_function;

    // get the return instruction
    // back track to storeInst
    // 120   store i32 1, i32* %3, align 4
    // 121   br label %22
    // 122
    // 123 22:                                               ; preds = %21, %19,
    // %13, %8 124   %23 = load i32, i32* %3, align 4 125   ret i32 %23

    Value *searchStoreTarget = nullptr;
    Instruction *returnInst = nullptr;
    for (inst_iterator ib = inst_begin(checkFunc); ib != inst_end(checkFunc);
         ib++) {
      Instruction *inst = &*ib;
      if (isa<ReturnInst>(inst)) {
        returnInst = inst;
        // BUG here, may be a phi instruction
        //
        if (isa<PHINode>(inst->getOperand(0))) {
          LOGERR("phi node here!\n");
          Instruction *t = dyn_cast<Instruction>(inst->getOperand(0));
          res.insert(t);
          return;
        } else if (isa<LoadInst>(inst->getOperand(0))) {
          LoadInst *ldInst = dyn_cast<LoadInst>(inst->getOperand(0));
          if (searchStoreTarget != nullptr) {
            LOGERR("Two ret instructions! This is rare!\n");
          }
          searchStoreTarget = ldInst->getOperand(0);
        }
        // BUG
        else { //
          res.insert(inst);
          return;
        }
      }
    }

    if (searchStoreTarget == nullptr) {
      LOGERR("No return instructions found!", funcName, "\n");
      return;
    }

    LOGERR("bfs searching start, ", *searchStoreTarget, "\n");
    // bfs search previous instructions to find storeInst
    BasicBlock *bb = returnInst->getParent();

    list<BasicBlock *> fifoQueue;
    set<BasicBlock *> visted;

    fifoQueue.push_back(bb);

    while (fifoQueue.size() > 0) {
      BasicBlock *currBB = fifoQueue.front();
      // errs() << *currBB << "=====\n";
      // errs() << visted.size() << "======\n";
      // errs() << fifoQueue.size() << "======\n";
      fifoQueue.pop_front();
      visted.insert(currBB);

      bool found = false;
      for (auto itr = currBB->begin(); itr != currBB->end(); itr++) {
        Instruction *bbinst = &*itr;
        if (isa<StoreInst>(bbinst)) {
          errs() << *(bbinst->getOperand(1)) << "\t" << *searchStoreTarget
                 << "\n";
          if (bbinst->getOperand(1) == searchStoreTarget) {
            LOGERR("bfs searching match, ", *bbinst, "\n");
            found = true;
            // variable match, then see if value match with rcf
            // we can not slice if there is no value match
            if (matchRCF(bbinst, rcf)) {
              res.insert(bbinst);
            }
          }
        }
      }
      if (!found) { // if found, no need to search again.
        LOGERR("bfs searching continue...", "\n");

        for (auto bitr = pred_begin(currBB); bitr != pred_end(currBB); ++bitr) {
          if (visted.find(*bitr) == visted.end()) // in case there is a circle
            fifoQueue.push_back(*bitr);
        }
      }

      // errs() << "After =====\n";
      // errs() << visted.size() << "======\n";
      // errs() << fifoQueue.size() << "======\n";
    }
  }

  // 1. get denied path
  // 2. get path conditions
  // 3. how/whether parameter affect the path conditions
  // This is obsolete now! Dont use it!
  void denyPathLookUp(string funcName, ResultCheckFunc *rcf) {
    // checkFuncAnalysis(funcName, rcf);
    errs() << "-----------------------Internal Check function "
              "analysis--------------------------\n";
    if (rcf == nullptr) {
      LOGERR("Sorry, can not find this function's rcf: ", funcName, "\n");
      return;
    }
    MiniGraphNode *graphNode = m_callGraph->getCallGraphNodeByName(funcName);
    // debug
    if (graphNode == nullptr) {
      LOGERR("Sorry, can not find this function: ", funcName, "\n");
      return;
    }
    // get the function
    Function *Caller = graphNode->_function;
    errs() << "Start look up: " << Caller->getName().str() << " ------\n";

    // Taint Analysis
    // LogicInfo mBottom; // empty
    // LogicInfo mInitialState; // empty
    // LogicAnalysis * ans = new LogicAnalysis(mBottom, mInitialState);
    // ans->runWorklistAlgorithm(Caller);

    Function *func = Caller;
    LogicAnalysis *ans;
    if (funcLogicAnalysisCache.count(func) != 0) {
      LOGINFO("Got previous cached function logic analysis.\n");
      ans = funcLogicAnalysisCache[func];
    } else {
      LogicInfo mBottom;       // empty
      LogicInfo mInitialState; // empty
      funcLogicAnalysisCache[func] = new LogicAnalysis(mBottom, mInitialState);
      funcLogicAnalysisCache[func]->runWorklistAlgorithm(func);
      ans = funcLogicAnalysisCache[func];
    }

    // last info is all info
    errs() << "---------Print taintinfo-------\n";
    // LogicInfo *lastinfo =
    ans->getLastInfo();
    errs() << "---------Finish taintinfo-------\n";

    // for field-sensitive only! no need if param are all not struct
    map<Instruction *, pair<Instruction *, int>> *fieldDepPairs =
        ans->getFieldDepPairs();
    // done with TA

    // Find all the parameter/arguments
    errs() << "---------- finding params-------\n";
    list<Value *> args;
    map<Value *, Value *> argStore;
    findArgs(Caller, args);
    // loop all the arguments
    // for (auto ab = Caller->arg_begin(), ae = Caller->arg_end(); ab != ae;
    // ++ab) {
    //     auto abb = &*ab;
    //     Value * a = dyn_cast<Value>(abb);
    //     // Instruction * vv = (Instruction *) a;
    //     args.push_back(a);
    // }
    // Find the block with return inst and get the operand
    set<Instruction *> affectedInstrs;
    map<Value *, set<Instruction *>> affectedInstrsList;
    BasicBlock *lastBlock;
    Value *returnVar;
    for (Function::iterator BB = Caller->begin(), BBE = Caller->end();
         BB != BBE; ++BB) {
      for (BasicBlock::iterator II = BB->begin(), IE = BB->end(); II != IE;
           ++II) {
        if (isa<ReturnInst>(&*II)) {
          lastBlock = &*BB; // why &* => convert iterator to BasicBlock*
          Instruction *i = &*II;
          returnVar = i->getOperand(0);
        }
        // Find which instruction stored the argument
        if (isa<StoreInst>(&*II)) {
          Instruction *I = &*II;
          // get store from
          Value *a = I->getOperand(0);
          // check if store from is part of the arg
          bool found = (find(args.begin(), args.end(), a) != args.end());
          if (found) {
            errs() << "Test found Arg: " << *(I->getOperand(1)) << "\n";
            // store from has value of store to
            argStore[I->getOperand(0)] = (I->getOperand(1));
            Instruction *ars = dyn_cast<Instruction>(I->getOperand(1));
            // get the branch instr affected by this arg
            affectedInstrs.clear();
            ans->getImpactedBranchInstr(ars, &affectedInstrs);
            // store to a map
            affectedInstrsList[I->getOperand(0)] = affectedInstrs;
          }
          // store %struct.request* %0, %struct.request** %4, align 8
          // %15 = load %struct.request*, %struct.request** %4, align 8
          // %16 = getelementptr inbounds %struct.request, %struct.request* %15,
          // i32 0, i32 0
          Instruction *argImpact =
              dyn_cast<Instruction>(I->getOperand(1)); //%struct.request** %4,

          //   bool foundInFields;
          for (auto mp : *fieldDepPairs) {
            if (mp.second.first == argImpact) {
              affectedInstrs.clear();
              ans->getImpactedBranchInstr(mp.first, &affectedInstrs);
              // store to a map
              affectedInstrsList[mp.first] = affectedInstrs;
            }
          }
        }
      }
    }
    // fifo queue for bts, final queue for result
    list<CFPath *> fifoQueue, finalQueue;
    // The fist node is base on the ret block
    CFPath *firstNode =
        new CFPath(lastBlock, true); // why is CPPath a pointer here
    // set return Var like % 37
    firstNode->targetVar = returnVar;
    // add current block (ret block) to the front of the list
    firstNode->addBlock(lastBlock);
    // first node for bfs
    fifoQueue.push_back(firstNode);
    // BFS start
    while (fifoQueue.size() > 0) {
      CFPath *currentNode = fifoQueue.front();
      fifoQueue.pop_front();
      // handle current node if store is on
      bool bk = false;
      if (currentNode->seekStore) {
        // if store wrong, push non, break
        for (BasicBlock::iterator II = currentNode->current->begin(),
                                  IE = currentNode->current->end();
             II != IE; ++II) {
          // find the store inst that give the return branch value
          if (isa<StoreInst>(&*II)) {
            errs() << "find Store\n";
            Instruction *i = &*II;
            errs() << *i << "\n";
            Instruction *vv = (Instruction *)currentNode->targetVar;
            errs() << *(vv->getOperand(0)) << "\n";
            errs() << *(i->getOperand(1)) << "\n";
            if (vv->getOperand(0) == i->getOperand(1)) {
              errs() << "match!\n";
              // check if this is not a deny path base on its rcf
              Value *first = i->getOperand(0);
              if (rcf->type == ResultCheckFuncType::RESULT_NE) {
                // if the value is not a constant then we cannot be sure
                if (isa<Constant>(first)) {
                  int cmp = dyn_cast<ConstantInt>(first)->getSExtValue();
                  if (cmp == rcf->denyVal) {
                    // accept path
                    bk = true;
                    break;
                  }
                }
              }
              if (rcf->type == ResultCheckFuncType::RESULT_EQ) {
                if (isa<Constant>(first)) {
                  int cmp = dyn_cast<ConstantInt>(first)->getZExtValue();
                  // TODO:change this
                  if (cmp != rcf->denyVal) {
                    // accept path
                    bk = true;
                    break;
                  }
                }
                // else{
                // FIXME: what if i am not a constant.
                // update target value?
                // }
              }
              currentNode->seekStore = false;
            }
          }
        }
      }
      if (bk) {
        continue;
      }

      // if has the right check func and no more pred. add to final queue
      int cnt = 0;
      // loop through all pred push ++
      for (auto pi = pred_begin(currentNode->current),
                pe = pred_end(currentNode->current);
           pi != pe; ++pi) {
        BasicBlock *blk = *pi;
        CFPath *tmp = new CFPath(blk, currentNode->seekStore);
        tmp->linkedlist = currentNode->linkedlist;
        tmp->targetVar = currentNode->targetVar;
        // link the block together.
        tmp->addBlock(blk);
        // can free currentNode now.
        fifoQueue.push_back(tmp);
        cnt++;
      }
      if (cnt == 0) {
        // push the block;
        finalQueue.push_back(currentNode);
      }
    }

    errs() << "---------------total path: " << finalQueue.size()
           << "------------\n";
    PostDominatorTree pdt(*Caller);
    // auto blocklist =  Caller->getBasicBlockList();
    auto bbList = &(Caller->getBasicBlockList());
    errs() << "--------start trim---\n";
    for (auto path : finalQueue) {
      // revise
      //   int blk_count = 0;
      // Fixme: maybe a map of path to branchlist
      list<Instruction *> branchList;
      //   path->targetVar; // return val
      errs() << "--------New round of backward search---\n";
      // backward search
      for (auto blk = path->linkedlist.rbegin(); blk != path->linkedlist.rend();
           ++blk) {

        BasicBlock *B = *blk; // how is this working...
        errs() << "------Each block: ";
        B->printAsOperand(errs(), false);
        errs() << "------\n";
        // if this block has conditional branch
        BranchInst *BInst = isBranchConditional(B);
        if (BInst != NULL) {
          errs() << "---conditional block---\n";
          // loop though from  A return block to this block B
          // forward search for ImPoDo
          for (auto bb = bbList->begin(); bb != bbList->end(); bb++) {
            BasicBlock *BB = &*bb; // how is this working...
            errs() << "Forword block: ";
            bb->printAsOperand(errs(), false);
            errs() << "\n";
            if (BB == B) {
              // continue
              errs() << "find equal block\n";
              continue;
            }
            // if A dominates B
            // should come here at least at the end, no else
            if (pdt.dominates(BB, B)) {
              errs() << "--find dominates---\n";
              if (!isBranchReturn(BB)) {
                if (!isBranchAffectResult(B, BB, branchList, path->targetVar,
                                          ans)) {
                  errs() << "Discard: Found branch that is not important\n";
                  // end the loop, next backard block
                  break;

                } else {
                  errs() << "Block affect, push branch in\n";
                  branchList.push_back(BInst);
                  break;
                }

              } else {
                // add this branch in.
                errs() << "Block is ret, push branch in\n";
                branchList.push_back(BInst);
                break;
              }
            }
          }
        }
        // get all the parameters that affect the branch in branchlist and the
        // return value.
      }
      for (auto branch : branchList) {
        errs() << "--------go through branch list---\n";
        if (isa<BranchInst>(&*branch)) {
          errs() << *branch << " \n";
          for (auto it : affectedInstrsList) {
            for (auto ins : it.second) {
              if (branch == ins) {
                errs() << "Find the arg that infected this branch\n";
                errs() << *ins << " \n";
                errs() << *it.first << " \n";
                path->affectTrimedArgs.insert(it.first);
              }
            }
          }
        }
      }
    }
    errs() << "--------end trim---\n";

    for (auto path : finalQueue) {
      errs() << "---------------print path------------\n";
      for (auto blk : path->linkedlist) {
        // print block level number
        blk->printAsOperand(errs(), false);
        errs() << "\n";
        for (BasicBlock::iterator II = blk->begin(), IE = blk->end(); II != IE;
             ++II) {
          Instruction *i = &*II;
          // FIXME: what if it is not every branch condition
          // change this part by taking it out and print it out
          if (isa<BranchInst>(&*II)) {
            // This way of getting inst should be fine for now.
            for (auto it : affectedInstrsList) {
              for (auto ins : it.second) {
                if (i == ins) {
                  // Value * arg= dyn_cast<Value>(it.first);
                  path->affectArgs.insert(it.first);
                }
              }
            }
          }

          errs() << *i << "\n";
        }
      }
      errs() << "Param that affact this path"
             << "\n";
      for (auto ag : path->affectArgs) {
        errs() << *ag << "\n";
      }

      errs() << "Trimed Param that affact this path"
             << "\n";
      for (auto ag : path->affectTrimedArgs) {
        errs() << *ag << "\n";
      }
    }
  }

  enum MyValueType { MyInt, MyBool };
  struct MyValue {
    int intval;
    bool boolval;
    int type;
    MyValue(int t, int value) {
      intval = value;
      type = t;
    }
    MyValue(int t, bool value) {
      boolval = value;
      type = t;
    }
    int gettype() { return type; }
    bool chooseTrueBranch() { return intval == 0; }
    string getPrintStr() {
      if (type == 0)
        return to_string(intval);
      if (type == 1)
        return to_string(boolval);
      return "bug!!!";
    }
  };

  void inferPathConditionsFromDenyPath(
      CFPath *denoPath, // the deny path
      map<Value *, set<Instruction *>>
          *affectedInstrsList) // keys arethe arg related instructions,
  {
    // put variables in the conditions,
    // see if can construct
    set<BasicBlock *> allBlocks;
    set<Instruction *> allInstructions;
    map<Instruction *, MyValue *> inferValues;
    copyList2Set(&allBlocks, &(denoPath->linkedlist));
    for (BasicBlock *bb : allBlocks) {
      for (BasicBlock::iterator II = bb->begin(), IE = bb->end(); II != IE;
           ++II) {
        Instruction *i = &*II;
        allInstructions.insert(i);
      }
    }
    // assign initial values based on branch conditions;
    // use work list algorithm to infer new values
    // if any newly inferred values are used in any instructions
    for (auto iter = denoPath->linkedlist.rbegin();
         iter != denoPath->linkedlist.rend(); iter++) { // from back
      // with in each blk, back track to assign values!
      BasicBlock *blk = *iter;
      if (blk->getUniqueSuccessor() != nullptr) {
        continue; // unconditional jump
      }
      Instruction *instr = blk->getTerminator();
      if (instr == nullptr) {
        LOGERR("This block does not have valid terminator!\n");
      }
      if (isa<BranchInst>(instr)) {

        BranchInst *inst = dyn_cast<BranchInst>(instr);
        int whichBranch = -1;
        for (unsigned i = 0; i < inst->getNumSuccessors(); ++i) {
          BasicBlock *bb = inst->getSuccessor(i);
          if (allBlocks.find(bb) != allBlocks.end()) {
            whichBranch = i;
            break;
          }
        }
        if (!(whichBranch == 0 || whichBranch == 1)) {
          LOGERR("no follow branch found! check the path!\n");
          return;
        }
        Value *cond = inst->getCondition();
        Instruction *newcond = dyn_cast<Instruction>(cond);
        if (newcond != nullptr)
          inferValues[newcond] = new MyValue(MyInt, whichBranch);
        else {
          LOGERR("VERY CONFUSING, condition is not instruction!\n");
        }
      }
    }

    // set up the list of instrs on the deny path backwards
    vector<Instruction *> rInstrPath;
    for (auto bbIter = denoPath->linkedlist.rbegin();
         bbIter != denoPath->linkedlist.rend(); bbIter++) {
      BasicBlock *bb = *bbIter;
      for (auto II = bb->rbegin(), IE = bb->rend(); II != IE; ++II) {
        Instruction *i = &*II;
        rInstrPath.push_back(i);
      }
    }
    // because it's linear, we just look backwards
    for (auto instrIter = rInstrPath.begin(); instrIter != rInstrPath.end();
         ++instrIter) {
      Instruction *inst = *instrIter;
      // LOGDEBUG(*inst, "\n");
      if (isa<CmpInst>(inst)) {
        // skip if we don't know the value
        if (inst == nullptr || inferValues.find(inst) == inferValues.end())
          continue;
        if (isa<ICmpInst>(inst)) {
          ICmpInst *icmpinst = dyn_cast<ICmpInst>(inst);

          Value *cmpsrc1 = icmpinst->getOperand(0);
          Value *cmpsrc2 = icmpinst->getOperand(1);
          if (isa<Instruction>(cmpsrc2) && isa<Constant>(cmpsrc1)) {
            auto tmp = cmpsrc2;
            cmpsrc2 = cmpsrc1;
            cmpsrc1 = tmp;
          }
          if (isa<Instruction>(cmpsrc1) && isa<Constant>(cmpsrc2)) {
            Instruction *cmpsrc1Instr = dyn_cast<Instruction>(cmpsrc1);
            if (isa<ConstantInt>(cmpsrc2)) {
              if ((icmpinst->getPredicate() == CmpInst::Predicate::ICMP_EQ) &&
                  inferValues[inst]->chooseTrueBranch()) {
                int cmpint = dyn_cast<ConstantInt>(cmpsrc2)->getZExtValue();
                inferValues[cmpsrc1Instr] = new MyValue(MyInt, cmpint);
              }
              if ((icmpinst->getPredicate() == CmpInst::Predicate::ICMP_NE) &&
                  !(inferValues[inst]->chooseTrueBranch())) {
                int cmpint = dyn_cast<ConstantInt>(cmpsrc2)->getZExtValue();
                inferValues[cmpsrc1Instr] = new MyValue(MyInt, cmpint);
              }
            } else if (isa<ConstantPointerNull>(cmpsrc2)) {
              // TODO
              LOGERR("Could not hanle ICMP with nullptr \n");

            } else {
              LOGERR("Could not hanle ICMP with non-int or nullptr constant\n");
            }
          }
        }
      } else if (isa<LoadInst>(inst)) {
        // skip if we don't know the value
        if (inst == nullptr || inferValues.find(inst) == inferValues.end())
          continue;
        // specific memory addres for us to load,
        // if we know what value will be loaded, the value at this location will
        // be temporarily stored!
        LoadInst *loadInst = dyn_cast<LoadInst>(inst);
        Value *src = loadInst->getOperand(0);
        if (!isa<Instruction>(src))
          continue;
        Instruction *srcInst = dyn_cast<Instruction>(src);
        if (inferValues.find(srcInst) != inferValues.end()) {
          LOGINFO(*srcInst, "already has been assigned value!\n");
        }
        inferValues[srcInst] =
            inferValues[inst]; // associate it with the same value
      } else if (isa<StoreInst>(inst)) {
        StoreInst *storeInst = dyn_cast<StoreInst>(inst);
        // we don't care about the currIdx itself

        Value *tgt = storeInst->getOperand(1); // tgt depends on src
        Value *src = storeInst->getOperand(0);

        Instruction *tgtInst = dyn_cast<Instruction>(tgt);
        Instruction *srcInst = dyn_cast<Instruction>(src);
        // skip if we don't know the value
        if (inst == nullptr || inferValues.find(tgtInst) == inferValues.end())
          continue;

        if (isa<Instruction>(tgt) && isa<Instruction>(src)) {
          inferValues[srcInst] = inferValues[tgtInst];
        }
        break;
      } else { // TODO : Do we need to handle other instructions?
      }
    }

    // print the inferred values;
    for (auto pr : inferValues) {
      errs() << *pr.first << "\t|\t" << pr.second->getPrintStr() << "\n";
    }
    // TODO: next, let's find parameters related values',
    // or idealy, the parameter's value
  }

  //==== Not used!
  void analyzeCheckFunctionByName(string funcName) {
    /*
    Now find all locations that one function is called!
    go to that basic block, see its descendents, basically, two or more path, it
    must be a tree structure of basic blocks. then count # of basic blocks # of
    call functions # of instructions
    */
    // errs() <<checkFuncNames.front() <<"should_have_sth\n";
    // TODO: this is
    MiniGraphNode *graphNode = m_callGraph->getCallGraphNodeByName(funcName);
    // debug
    if (graphNode == nullptr) {
      LOGERR("Sorry, can not find this function: ", funcName, "\n");
      return;
    }

    int lcnt = 0;

    for (auto instr : graphNode->_callSite) {
      errs() << "\n"; // keep this to separate for differnet call sites
      std::cout << funcName << ":" << lcnt << endl;
      LOGINFO("===================\n", funcName, " Usage #", lcnt++,
              "\tby: ", *instr, "\n");

      // 1.0 start from the function
      Function *func = instr->getParent()->getParent();

      TaintAnalysis *ans;
      // 2.0 get all taint analysis in this func

      // 2.1 read from cache, to see if has been cached before
      if (funcTaintAnalysisCache.count(func) != 0) {
        LOGINFO("Got previous cached function taint analysis.\n");
        ans = funcTaintAnalysisCache[func];
      } else {
        // 2.2
        TaintInfo mBottom;       // empty
        TaintInfo mInitialState; // empty
        funcTaintAnalysisCache[func] =
            new TaintAnalysis(mBottom, mInitialState);
        funcTaintAnalysisCache[func]->runWorklistAlgorithm(func);
        // only keep the last taintInfo
        funcTaintAnalysisCache[func]->freeAllInfoButLastInfo();

        // just read from cache
        ans = funcTaintAnalysisCache[func];
      }

      // 3.0, get all branch instructions that has been impacted by original
      // instr
      set<Instruction *> affectedInstrs;
      // now we get all conditional branches
      ans->getImpactedBranchInstr(instr, &affectedInstrs);

      if (affectedInstrs.size() == 0) {
        LOGERR("THIS IS RARE, no affected branch instructions, next!\n");
        continue;
      }

      Instruction *firstImpactedBranchInst = *(affectedInstrs.begin());
      // ugly hack, find the branchInst that is later than original instr!!!
      // don't know how to improve yet, but the original taint seems works fine
      // :(
      BasicBlock *parentBB = instr->getParent();
      set<BasicBlock *> visited;

      firstImpactedBranchInst =
          getNextBranchInstBFS(parentBB, &affectedInstrs, &visited);
      if (firstImpactedBranchInst == nullptr) {
        LOGERR("Can't find the next branch instr after original inst, next!\n");
        continue;
      }
      std::cout << "[tmp] get the first impacted branch instructions\n";

      /*
       * Now begin to do the PostDominatorAnalysis
       * see if the true/false branch has a PDT, if so, just count to the
       * PDT point, usually, this is a ret instruction.
       * However, if not critical errors that make program early exit, may
       * not have much differences
       */
      BasicBlock *dombb = nullptr;
      if (enablePDT) {
        BasicBlock *tbb = firstImpactedBranchInst->getSuccessor(0);
        BasicBlock *fbb = firstImpactedBranchInst->getSuccessor(1);

        PostDominatorTree pdt(*func);
        // get the dominated bb
        dombb = pdt.findNearestCommonDominator(tbb, fbb);
      }

      analyzeBasicBlock(firstImpactedBranchInst->getParent(), m_profiler,
                        dombb);
      std::cout << "[tmp] done with analysis\n";

      // delete ans;
    }
  }
  //==== Not used!
  Instruction *getNextCallInstBFS(BasicBlock *bb,
                                  set<Instruction *> *affectedInstrs,
                                  set<BasicBlock *> *visited) {
    list<BasicBlock *> queue;
    queue.push_back(bb);
    visited->insert(bb);
    while (!queue.empty()) {
      BasicBlock *fbb = queue.front();
      queue.pop_front();

      for (auto it = fbb->begin(); it != fbb->end(); ++it) {
        if (affectedInstrs->count(&*it) != 0) {
          return &*it;
        }
      }
      // not found keep searching...

      for (auto I = succ_begin(fbb), E = succ_end(fbb); I != E; I++) {
        // for each successor, insert current block
        if (visited->find(*I) == visited->end()) {
          visited->insert(*I);
          queue.push_back(*I);
        }
      }
    }
    return nullptr;
  }
  //==== Not used!
  Instruction *getNextBranchInstBFS(BasicBlock *bb,
                                    set<Instruction *> *affectedInstrs,
                                    set<BasicBlock *> *visited) {
    list<BasicBlock *> queue;
    queue.push_back(bb);
    visited->insert(bb);
    while (!queue.empty()) {
      BasicBlock *fbb = queue.front();
      queue.pop_front();

      if (affectedInstrs->count(fbb->getTerminator()) == 0) {
        for (auto I = succ_begin(fbb), E = succ_end(fbb); I != E; I++) {
          // for each successor, insert current block
          if (visited->find(*I) == visited->end()) {
            visited->insert(*I);
            queue.push_back(*I);
          }
        }
      } else {
        return fbb->getTerminator();
      }
    }

    return nullptr;
  }
  //==== Not used!
  Instruction *getNextBranchInst(BasicBlock *bb,
                                 set<Instruction *> *affectedInstrs,
                                 set<BasicBlock *> *visited) {
    if (visited->find(bb) != visited->end()) {
      return nullptr;
    }
    visited->insert(bb);
    if (affectedInstrs->count(bb->getTerminator()) == 0) {
      // not found, BFS to find next?
      list<Instruction *> res;

      for (auto I = succ_begin(bb), E = succ_end(bb); I != E; I++) {
        // for each successor, insert current block
        BasicBlock *succ = *I;
        res.push_back(getNextBranchInst(succ, affectedInstrs, visited));
      }

      for (auto &it : res) {
        if (it != nullptr) {
          return it; // found in one of its successors
        }
      }
      return nullptr;
    } else {

      return bb->getTerminator();
    }
  }
  //==== Not used!
  bool getFunctionsInBB(BasicBlock *BB, list<Function *> &blockFunctions) {
    bool containExitFunc = false;
    for (BasicBlock::iterator II = BB->begin(), IE = BB->end(); II != IE;
         ++II) {
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
        if (m_exitFuncs.find(callee) != m_exitFuncs.end()) {
          containExitFunc = true;
          continue;
        }
        blockFunctions.push_back(callee);
      }
    }
    return containExitFunc;
  }
  //==== Not used!
  int helperAnalyzeBasicBlock(
      BasicBlock *bb,
      BasicBlock
          *terminateBB, // the original other branch, if meet it early return
      set<BasicBlock *> &visited, FunctionProfiler *profiler,
      BasicBlock *dombb = nullptr) {
    // look at all
    if (visited.find(bb) != visited.end()) {
      // this bb has been visited
      return 0;
    }
    if (dombb != nullptr && dombb == bb) {
      // return 0 if we reach the dominator bb
      return 0;
    }

    /*  deal with cases as follows, use original the other branch, stop when
       reach that point. Question: can this cause problems that if the deny
       branch post-dominate not-denied branch? Ans: I think it is rare because
       the the not-denied branch should be the longer one. rv =
       apr_file_read_full(pid_file, buf, BUFFER_SIZE - 1, &bytes_read); if (rv
       != APR_SUCCESS && rv != APR_EOF) { return rv;}
    */
    if (terminateBB != nullptr && bb == terminateBB) {
      LOGERR("BAD!! Get the other branch in the successors! \n");
      return 0;
    }
    int res = 0;

    list<Function *> blockFunctions;
    bool containExitFunc = getFunctionsInBB(bb, blockFunctions);

    switch (currAnalysisType) {
    case CNT_BASIC_BLOCK:
      res = 1;
      break;
    case CNT_INSTRUCTION:
      res = std::distance(bb->begin(), bb->end());
      break;
    case CNT_FUNCCALL_BLOCK: {
      res = 1; // add current block
      for (auto I = blockFunctions.begin(), E = blockFunctions.end(); I != E;
           I++) {
        // we ignore the defined library functions like log_error(),
        // handle_error(), if (m_libFuncs.count(*I) != 0) {
        //     continue;
        // }
        res += profiler->func2BlockCnt[*I];
      }
    } break;
    case CNT_FUNCCALL_INSTRUCTION: {
      res = std::distance(bb->begin(), bb->end()); // add current block
      for (auto I = blockFunctions.begin(), E = blockFunctions.end(); I != E;
           I++) {
        // we ignore the defined library functions like log_error(),
        // handle_error(), if (m_libFuncs.count(*I) != 0) { continue;
        // }
        res += profiler->func2InstrCnt[*I];
      }
    } break;
    default:
      res = 0;
    }
    visited.insert(bb);

    // if the current basic block contains a exit func that will be called
    // stop now
    if (containExitFunc) {
      return res;
    }

    for (auto I = succ_begin(bb), E = succ_end(bb); I != E; I++) {
      BasicBlock *succ = *I;
      res +=
          helperAnalyzeBasicBlock(succ, terminateBB, visited, profiler, dombb);
    }

    return res;
  }
  //==== Not used!
  void analyzeBasicBlock(BasicBlock *bb, FunctionProfiler *profiler,
                         BasicBlock *dombb = nullptr) {
    // errs() << "====BEGIN ANALYSIS===\n"
    errs() << "inside this function: " << bb->getParent()->getName() << "\n";
    // store result on all branches;
    list<int> resSucc;

    // start from this basick block, see how many basic blocks on each side.
    int numSucc = std::distance(succ_begin(bb), succ_end(bb));
    LOGINFO("find ", numSucc, " successors\n");

    if (numSucc != 2) {
      LOGERR("find ", numSucc, " successors, return early\n");
      return;
    }

    // TODO hanle the cases that there is no branches following the current
    // block DONE 0. currently only search for the next branch within the
    // function-> which could
    //    probably be the AC check result :)
    // 1. future may use data dependency to find the

    // if (numSucc == 1) {
    //     while (bb->getUniqueSuccessor() != nullptr) { // only slight
    //     difference between the
    //         bb = bb->getUniqueSuccessor();
    //     }
    //     numSucc = std::distance(succ_begin(bb), succ_end(bb));
    //     errs() << "Keep looking -> find next block with "<< numSucc <<"
    //     successors\n"; if (numSucc == 0) { // keep iterating to the end of
    //     the function :(, still no branches
    //         return;
    //     }
    // }

    // we Just keep this to show how much can be improved by the basic block
    // analysis.
    for (auto I = succ_begin(bb), E = succ_end(bb); I != E; I++) {
      // for each successor, insert current block
      set<BasicBlock *> visited;
      visited.insert(bb);

      BasicBlock *succ = *I;
      resSucc.push_back(
          helperAnalyzeBasicBlock(succ, nullptr, visited, profiler, dombb));
    }

    // true branch and false branch, record to prune the results

    BasicBlock *tbb = bb->getTerminator()->getSuccessor(0);
    BasicBlock *fbb = bb->getTerminator()->getSuccessor(1);

    set<BasicBlock *> visited;
    visited.insert(bb);
    resSucc.push_back(
        helperAnalyzeBasicBlock(tbb, fbb, visited, profiler, dombb));

    visited.clear();
    visited.insert(bb);
    resSucc.push_back(
        helperAnalyzeBasicBlock(fbb, tbb, visited, profiler, dombb));

    printList(&resSucc);
    return;
  }

}; // end of analyzer functions

#endif