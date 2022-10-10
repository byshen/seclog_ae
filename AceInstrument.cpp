// All the magic starts here.
//
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include <chrono>

#include "ACFuncAnalyzer.h"
#include "AceInstrument.h"
#include "llvm/IR/Module.h"

// #define ANALYZE_FUNCTION_INSTRS

using namespace llvm;
// global variables
MiniCallGraph *g_callGraph;

namespace {
struct AceInstrument : public ModulePass {
  // dummy variable
  static char ID;
  // the one and only module/config
  Module *_module;
  Config *_conf;

  // a list of AC check function names, used for matching
  list<string> checkFuncNames;
  list<string> exitFuncNames;
  // store the log functions :)
  list<string> libFuncNames;
  list<StructFieldPair *> checkStructFuncPairs;

  map<string, ResultCheckFunc *> checkFuncWResult;
  map<StructFieldPair *, ResultCheckFunc *> checkStructFuncWResult;

  AceInstrument() : ModulePass(ID) { _conf = new Config(); }

  // deprecated
  void init() {
    if (_conf->_check_file != "")
      initFuncs(_conf->_check_file, checkFuncNames);
    if (_conf->_exit_file != "")
      initFuncs(_conf->_exit_file, exitFuncNames);
    if (_conf->_libcall_file != "")
      initFuncs(_conf->_libcall_file, libFuncNames); // for the log functions
    if (_conf->_struct_file != "")
      initStructFuncs(_conf->_struct_file, checkStructFuncPairs);
  }

  void initWithRetVal() {
    if (_conf->_check_file != "")
      initFuncsWithRetval(_conf->_check_file, checkFuncWResult);
    if (_conf->_exit_file != "")
      initFuncs(_conf->_exit_file, exitFuncNames);
    if (_conf->_libcall_file != "")
      initFuncs(_conf->_libcall_file, libFuncNames); // for the log functions
    // Ignore for now
    if (_conf->_struct_file != "")
      initStructFuncsWithRetval(_conf->_struct_file, checkStructFuncWResult);
  }

  bool runOnModule(Module &M) override {
    // Open write file
    std::error_code EC;
    this->_module = &M;

    std::chrono::high_resolution_clock::time_point t1 =
        std::chrono::high_resolution_clock::now();

    _conf->initConfig(CONFIG_FILE);

    // Call graph should be build before call initialization and any analysis
    std::cout << "Initializing call graph ...\n";
    g_callGraph = new MiniCallGraph(M);

    // init functions for analysis
    // init();
    // This new function takes result check func compare with
    initWithRetVal();

    ACFuncAnalyzer *analyzer =
        new ACFuncAnalyzer(&M, exitFuncNames, libFuncNames);

    // #ifdef ANALYZE_FUNCTION_INSTRS
    //     analyzer->setACFuncAnalysisType(CNT_FUNCCALL_INSTRUCTION);

    //     for (auto funcName : checkFuncNames) {
    //       analyzer->analyzeCheckFunctionByName(funcName);
    //     }
    //     analyzer->analyzeStructFuncPairs(checkStructFuncPairs);
    // #endif

    for (auto fp : checkFuncWResult) {
      // denied branch =

      // 0. get the denied branch at all call sites
      //

      std::cout << "analyzing access check func" << fp.first << "\n";
      analyzer->deniedBranchLookUpByName(fp.first, fp.second);

      // search for relevant parameters
      // analyzer->denyPathLookUp(fp.first, fp.second);
      // analyzer->checkFuncAnalysis(fp.first, fp.second);
    }

    analyzer->init_m_structCallMap(checkStructFuncWResult);
    for (auto sfp : checkStructFuncWResult) {
      std::cout << "analyzing struct check func " << sfp.first << "\n";
      analyzer->deniedBranchLookUpByStructFieldMap(sfp.first, sfp.second);
    }

    analyzer->writeCloneModule(_conf->_softwareName);

    std::chrono::high_resolution_clock::time_point t2 =
        std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> time_span = t2 - t1;

    errs() << "Time in chrono," << time_span.count() << "seconds";

    delete analyzer;
    return false;
  }

}; // end of struct TestPass
} // end of anonymous namespace

char AceInstrument::ID = 0;
static RegisterPass<AceInstrument> X("ace-instrument", "The Ace Instrument",
                                     false /* Only looks at CFG */,
                                     false /* Analysis Pass */);
