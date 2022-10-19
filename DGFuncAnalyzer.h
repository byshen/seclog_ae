// // Implementing DG

// // Jun 8 2020
// #ifndef _DGFuncAnalyzer_H_
// #define _DGFuncAnalyzer_H_

// // #define AC_FUNC_ANALYZER_DEBUG

// #include "dg/llvm/LLVMDependenceGraph.h"
// #include "dg/llvm/LLVMDependenceGraphBuilder.h"
// #include "dg/llvm/LLVMNode.h"
// #include "dg/llvm/LLVMSlicer.h"

// #include "Common.h"
// #include "Struct.h"
// #include "Utils.h"
// // #include "ACFuncAnalyzer.h"

// // #include "MyCallGraph.h"
// // #include "ResultCheckFunc.h"

// // #include "LogicRelationAnalysis.h"
// // #include "DataDependencyAnalyzer.h" // TaintAnalysis
// // #include "llvm/Analysis/PostDominators.h"
// // #include "llvm-slicer.h"

// #include "llvm/Bitcode/BitcodeReader.h"
// #include "llvm/Bitcode/BitcodeWriter.h"

// #include "llvm/IRReader/IRReader.h"
// #include "llvm/Support/CommandLine.h"
// #include "llvm/Support/PrettyStackTrace.h"
// #include "llvm/Support/Signals.h"
// #include "llvm/Support/SourceMgr.h"
// #include "llvm/Support/raw_os_ostream.h"

// // extern MiniCallGraph* g_callGraph;

// using namespace dg;

// class ModuleWriter {
//   const string funcName;
//   llvm::Module *M;

// public:
//   int cleanAndSaveModule(bool should_verify_module = true) {
//     // remove unneeded parts of the module
//     removeUnusedFromModule();

//     // fix linkage of declared functions (if needs to be fixed)
//     makeDeclarationsExternal();

//     return saveModule(should_verify_module);
//   }

//   int saveModule(bool should_verify_module = true) {
//     // if (should_verify_module)
//     //     return verifyAndWriteModule();
//     // else
//     return writeModule();
//   }

//   bool writeModule() {
//     // compose name if not given
//     std::string fl;
//     // if (!options.outputFile.empty()) {
//     //     fl = options.outputFile;
//     // } else {
//     //     fl = options.inputFile;
//     //     replace_suffix(fl, ".sliced");
//     // }

//     fl = "sliced.bc";

//     // open stream to write to
//     std::ofstream ofs(fl);
//     llvm::raw_os_ostream ostream(ofs);

//     // write the module
//     errs() << "[llvm-slicer] saving sliced module to: " << fl.c_str() <<
//     "\n";

// #if (LLVM_VERSION_MAJOR > 6)
//     llvm::WriteBitcodeToFile(*M, ostream);
// #else
//     llvm::WriteBitcodeToFile(M, ostream);
// #endif

//     return true;
//   }

//   ModuleWriter(const string &o, llvm::Module *m) : funcName(o), M(m) {}

//   void makeDeclarationsExternal() {
//     using namespace llvm;

//     // iterate over all functions in module
//     for (auto &F : *M) {
//       if (F.size() == 0) {
//         // this will make sure that the linkage has right type
//         F.deleteBody();
//       }
//     }
//   }

//   void removeUnusedFromModule() {
//     bool fixpoint;

//     do {
//       fixpoint = _removeUnusedFromModule();
//     } while (fixpoint);
//   }

//   template <typename T> bool array_match(llvm::StringRef name, const T
//   &names) {
//     for (auto &n : names) {
//       if (name.equals(n))
//         return true;
//     }

//     return false;
//   }

//   bool _removeUnusedFromModule() {
//     using namespace llvm;
//     // do not slice away these functions no matter what
//     // FIXME do it a vector and fill it dynamically according
//     // to what is the setup (like for sv-comp or general..)
//     const char *keep[] = {funcName.c_str()};

//     // when erasing while iterating the slicer crashes
//     // so set the to be erased values into container
//     // and then erase them
//     std::set<Function *> funs;
//     std::set<GlobalVariable *> globals;
//     std::set<GlobalAlias *> aliases;

//     for (auto I = M->begin(), E = M->end(); I != E; ++I) {
//       Function *func = &*I;
//       if (array_match(func->getName(), keep))
//         continue;

//       // if the function is unused or we haven't constructed it
//       // at all in dependence graph, we can remove it
//       // (it may have some uses though - like when one
//       // unused func calls the other unused func
//       if (func->hasNUses(0))
//         funs.insert(func);
//     }

//     for (auto I = M->global_begin(), E = M->global_end(); I != E; ++I) {
//       GlobalVariable *gv = &*I;
//       if (gv->hasNUses(0))
//         globals.insert(gv);
//     }

//     for (GlobalAlias &ga : M->getAliasList()) {
//       if (ga.hasNUses(0))
//         aliases.insert(&ga);
//     }

//     for (Function *f : funs)
//       f->eraseFromParent();
//     for (GlobalVariable *gv : globals)
//       gv->eraseFromParent();
//     for (GlobalAlias *ga : aliases)
//       ga->eraseFromParent();

//     return (!funs.empty() || !globals.empty() || !aliases.empty());
//   }
// };

// struct SlicerOptions {
//   dg::llvmdg::LLVMDependenceGraphOptions dgOptions{};

//   // FIXME: get rid of this once we got the secondary SC
//   std::vector<std::string> additionalSlicingCriteria{};

//   // bodies of these functions will not be sliced
//   std::vector<std::string> preservedFunctions{};

//   // slice away also the slicing criteria nodes
//   // (if they are not dependent on themselves)
//   bool removeSlicingCriteria{false};

//   // do we perform forward slicing?
//   bool forwardSlicing{false};

//   std::string slicingCriteria{};
//   std::string secondarySlicingCriteria{};
//   std::string inputFile{};
//   std::string outputFile{};
// };

// class MySlicer {
//   llvm::Module *M{};
//   const SlicerOptions &_options;

//   dg::llvmdg::LLVMDependenceGraphBuilder _builder;
//   std::unique_ptr<dg::LLVMDependenceGraph> _dg{};

//   dg::llvmdg::LLVMSlicer slicer;
//   uint32_t slice_id = 0;
//   bool _computed_deps{false};

// public:
//   MySlicer(llvm::Module *mod, const SlicerOptions &opts)
//       : M(mod), _options(opts), _builder(mod, _options.dgOptions) {
//     errs() << "Building slicer??\n";
//     if (mod->getFunction("main") == nullptr) {
//       errs() << "ERROR!\n";
//       assert(false);
//     }
//     assert(mod && "Need module");
//   }

//   const dg::LLVMDependenceGraph &getDG() const { return *_dg.get(); }
//   dg::LLVMDependenceGraph &getDG() { return *_dg.get(); }

//   // Mirror LLVM to nodes of dependence graph,
//   // No dependence edges are added here unless the
//   // 'compute_deps' parameter is set to true.
//   // Otherwise, dependencies must be computed later
//   // using computeDependencies().
//   bool buildDG(bool compute_deps = false) {
//     _dg = std::move(_builder.constructCFGOnly());

//     if (!_dg) {
//       llvm::errs() << "Building the dependence graph failed!\n";
//       return false;
//     }

//     if (compute_deps)
//       computeDependencies();

//     return true;
//   }

//   // Explicitely compute dependencies after building the graph.
//   // This method can be used to compute dependencies without
//   // calling mark() afterwards (mark() calls this function).
//   // It must not be called before calling mark() in the future.
//   void computeDependencies() {
//     assert(!_computed_deps && "Already called computeDependencies()");
//     // must call buildDG() before this function
//     assert(_dg && "Must build dg before computing dependencies");

//     _dg = _builder.computeDependencies(std::move(_dg));
//     _computed_deps = true;

//     const auto &stats = _builder.getStatistics();
//     llvm::errs() << "[llvm-slicer] CPU time of pointer analysis: "
//                  << double(stats.ptaTime) / CLOCKS_PER_SEC << " s\n";
//     llvm::errs() << "[llvm-slicer] CPU time of reaching definitions analysis:
//     "
//                  << double(stats.rdaTime) / CLOCKS_PER_SEC << " s\n";
//     llvm::errs() << "[llvm-slicer] CPU time of control dependence analysis: "
//                  << double(stats.cdaTime) / CLOCKS_PER_SEC << " s\n";
//   }

//   // Mark the nodes from the slice.
//   // This method calls computeDependencies(),
//   // but buildDG() must be called before.
//   bool mark(std::set<dg::LLVMNode *> &criteria_nodes) {
//     assert(_dg && "mark() called without the dependence graph built");
//     assert(!criteria_nodes.empty() && "Do not have slicing criteria");

//     // dg::debug::TimeMeasure tm;

//     // compute dependece edges
//     computeDependencies();

//     // unmark this set of nodes after marking the relevant ones.
//     // Used to mimic the Weissers algorithm
//     std::set<dg::LLVMNode *> unmark;

//     if (_options.removeSlicingCriteria)
//       unmark = criteria_nodes;

//     _dg->getCallSites(_options.additionalSlicingCriteria, &criteria_nodes);

//     for (auto &funcName : _options.preservedFunctions)
//       slicer.keepFunctionUntouched(funcName.c_str());

//     slice_id = 0xdead;

//     // // tm.start();
//     // llvm::errs() << "here\n";
//     for (dg::LLVMNode *start : criteria_nodes) {
//       slice_id = slicer.mark(start, slice_id, _options.forwardSlicing);
//       LOGINFO("slice id after mark", slice_id, "\n");
//     }

//     // llvm::errs() << "done mark\n";
//     assert(slice_id != 0 && "Somethig went wrong when marking nodes");

//     // if we have some nodes in the unmark set, unmark them
//     for (dg::LLVMNode *nd : unmark)
//       nd->setSlice(0);

//     // tm.stop();
//     // tm.report("[llvm-slicer] Finding dependent nodes took");

//     return true;
//   }

//   bool slice() {
//     assert(_dg && "Must run buildDG() and computeDependencies()");
//     assert(slice_id != 0 && "Must run mark() method before slice()");

//     // dg::debug::TimeMeasure tm;

//     // tm.start();
//     slicer.slice(_dg.get(), nullptr, slice_id);

//     // tm.stop();
//     // tm.report("[llvm-slicer] Slicing dependence graph took");

//     dg::SlicerStatistics &st = slicer.getStatistics();
//     llvm::errs() << "[llvm-slicer] Sliced away " << st.nodesRemoved << " from
//     "
//                  << st.nodesTotal << " nodes in DG\n";

//     return true;
//   }

//   bool sliceByFunctionName(string fname) {
//     // Refer LLVMSlicer.h for how to use sliceGraph() and LLVMDependenceGraph
//     // only slice intra procedurally for the function to save time!!!!

//     auto constructedFucntions = getConstructedFunctions();
//     for (auto it : constructedFucntions) {
//       Function *f = dyn_cast<Function>(it.first);
//       LOGINFO(f->getName(), "\n");

//       if (fname == f->getName()) {
//         LOGERR("great!", slice_id, "\n");
//         slicer.sliceGraph(it.second, slice_id);
//       }
//     }
//     return true;
//   }

//   ///
//   // Create new empty main in the module. If 'call_entry' is set to true,
//   // then call the entry function from the new main (if entry is not main),
//   // otherwise the main is going to be empty
//   /*
//   bool createEmptyMain(bool call_entry = false)
//   {
//       llvm::LLVMContext& ctx = M->getContext();
//       llvm::Function *main_func = M->getFunction("main");
//       if (!main_func) {
//           auto C = M->getOrInsertFunction("main",
//                                           llvm::Type::getInt32Ty(ctx)
// #if LLVM_VERSION_MAJOR < 5
//                                           , nullptr
// #endif // LLVM < 5
//                                           );
// #if LLVM_VERSION_MAJOR < 9
//           if (!C) {
//               llvm::errs() << "Could not create new main function\n";
//               return false;
//           }

//           main_func = llvm::cast<llvm::Function>(C);
// #else
//           main_func = llvm::cast<llvm::Function>(C.getCallee());
// #endif
//       } else {
//           // delete old function body
//           main_func->deleteBody();
//       }

//       assert(main_func && "Do not have the main func");
//       assert(main_func->size() == 0 && "The main func is not empty");

//       // create new function body
//       llvm::BasicBlock* blk = llvm::BasicBlock::Create(ctx, "entry",
//       main_func);

//       if (call_entry && _options.dgOptions.entryFunction != "main") {
//           llvm::Function *entry =
// M->getFunction(_options.dgOptions.entryFunction); assert(entry && "The entry
// function is not present in the module");

//           // TODO: we should set the arguments to undef
//           llvm::CallInst::Create(entry, "entry", blk);
//       }

//       llvm::Type *Ty = main_func->getReturnType();
//       llvm::Value *retval = nullptr;
//       if (Ty->isIntegerTy())
//           retval = llvm::ConstantInt::get(Ty, 0);
//       llvm::ReturnInst::Create(ctx, retval, blk);

//       return true;
//   }
//   */
// };

// #endif