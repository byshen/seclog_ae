//

#ifndef LLVMPASS_MYCALLGRAPH_H
#define LLVMPASS_MYCALLGRAPH_H

#include "Common.h"
#include "Utils.h"
#include "llvm/IR/Module.h"

using namespace std;
using namespace llvm;

struct MiniGraphNode {

  MiniGraphNode(Function *F) { _function = F; }

  MiniGraphNode(Function *F, Instruction *instr) {
    _function = F;
    _callSite.push_back(instr);
  }

  ~MiniGraphNode() { _callees.clear(); }

  void addCallSite(Instruction *instr) { _callSite.push_back(instr); }
  // FIXME: this is the func
  Function *_function;
  list<Instruction *> _callSite;
  set<MiniGraphNode *> _callees;
};

struct MiniCallGraph {
  map<Function *, MiniGraphNode *> *_miniCallGraph;
  map<string, Function *> *demangledNameToFuncPtr;

  MiniCallGraph(Module &M) {
    _miniCallGraph = new map<Function *, MiniGraphNode *>();
    runOnModule(M);

    demangledNameToFuncPtr = new map<string, Function *>();
    // initialized in the beginning,
    // cache the results for later use
    initDemangledNameMap();

    printGraph();
  }

  ~MiniCallGraph() { destroy(_miniCallGraph); }

  MiniGraphNode *getCallGraphNode(Function *F) {
    return _miniCallGraph->operator[](F);
  }

  void initDemangledNameMap() {
    auto I = _miniCallGraph->begin();
    auto IE = _miniCallGraph->end();
    for (; I != IE; I++) {
      string demangledName = getDemangledName(I->first->getName());

      demangledNameToFuncPtr->operator[](demangledName) = I->first;
    }
  }

  Function *getFuncPtrByDemangledName(string funcName) {
    if (demangledNameToFuncPtr->count(funcName) == 0)
      return nullptr;
    return demangledNameToFuncPtr->operator[](funcName);
  }

  MiniGraphNode *getCallGraphNodeByName(string funcName) {
    if (demangledNameToFuncPtr->count(funcName) == 0) {
      LOGERR("Could not find the name in Call Graph ", funcName, "\n");
      return nullptr;
    }
    Function *func = demangledNameToFuncPtr->operator[](funcName);
    return _miniCallGraph->operator[](func);
  }

  void addNode(Function *F) {
    if (_miniCallGraph->count(F) == 0) {
      _miniCallGraph->operator[](F) = new MiniGraphNode(F);
    }
  }

  void addNode(Function *F, Instruction *instr) {
    if (_miniCallGraph->count(F) == 0) {
      _miniCallGraph->operator[](F) = new MiniGraphNode(F, instr);
    } else { // if the node is already there, just add a callsite
      _miniCallGraph->operator[](F)->addCallSite(instr);
    }
  }

  void addEdge(Function *Caller, Function *Callee) {
    addNode(Caller);
    addNode(Callee);

    _miniCallGraph->operator[](Caller)->_callees.insert(
        _miniCallGraph->operator[](Callee));
  }

  void addEdge(Function *Caller, Function *Callee, Instruction *CalleeInstr) {
    addNode(Caller);
    addNode(Callee, CalleeInstr);
    _miniCallGraph->operator[](Caller)->_callees.insert(
        _miniCallGraph->operator[](Callee));
  }

  void BFS(set<Function *> *seeds, set<Function *> *results) {
    set<Function *>::iterator sIter = seeds->begin(), sEnd = seeds->end();
    for (; sIter != sEnd; sIter++) {
      set<Function *> tmpresults;
      BFS(*sIter, &tmpresults);

      copySet(results, &tmpresults);
    }
  }

  void BFS(Function *seed, set<Function *> *results) {
    list<Function *> fifoQueue;
    fifoQueue.push_back(seed);

    while (fifoQueue.size() > 0) {
      Function *curr = fifoQueue.front();
      fifoQueue.pop_front();

      if (results->count(curr) == 0) {
        results->insert(curr);

        set<MiniGraphNode *> neighbors =
            _miniCallGraph->operator[](curr)->_callees;
        set<MiniGraphNode *>::iterator iter = neighbors.begin(),
                                       end = neighbors.end();
        for (; iter != end; iter++) {
          fifoQueue.push_back((*iter)->_function);
        }
      }
    }
  }

  void runOnModule(Module &M) {
    for (auto I = M.begin(), E = M.end(); I != E; ++I) {
      addToCallGraph(dyn_cast<Function>(I));
    }
  }

  void printGraph() { 
    printGraph(_miniCallGraph); 
    LOGDEBUG("----------DEMANGLED FUNC NAME------------\n");
    for (auto const &x : *demangledNameToFuncPtr) {
      LOGDEBUG(x.first, "\t->\t", x.second->getName(), "\n");
    }  
  }

private:
  void printGraph(map<Function *, MiniGraphNode *> *graph) {
    LOGDEBUG("----------PRINT GRAPH------------\n");

    map<Function *, MiniGraphNode *>::iterator iter = graph->begin(),
                                               end = graph->end();
    for (; iter != end; iter++) {
      LOGDEBUG(iter->first->getName(), " | ");
      MiniGraphNode *node = iter->second;
      set<MiniGraphNode *> neighbors = node->_callees;
      set<MiniGraphNode *>::iterator niter = neighbors.begin(),
                                     nend = neighbors.end();

      for (; niter != nend; niter++) {

        auto callSites = (*niter)->_callSite;
        LOGDEBUG((*niter)->_function->getName(), ":", callSites.size(), " ");

        // show all call sites, buggy
        // if (callSites.size()==0) {
        //     errs() << "must be something wrong\n";
        //     continue;
        // }
        // for (auto I = callSites.begin(), E = callSites.end(); I!=E; ++I) {
        //     auto instr = dyn_cast<Instruction>(*I);
        //     unsigned linenum;
        //     StringRef filename;
        //     getInstSrcLocation(, linenum, filename);
        //     errs() << *I << filename <<":"<<linenum<<" ";
        // }
      }
      LOGDEBUG("\n");
    }
    LOGDEBUG("----------PRINT GRAPH END------------\n");
  }

  void addToCallGraph(Function *Caller) {
    // Look for calls by this function.
    for (Function::iterator BB = Caller->begin(), BBE = Caller->end();
         BB != BBE; ++BB) {
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
          if (!callee || !stripIntrinsic(callee, realName)) {
            continue;
          }

          // unsigned linenum;
          // StringRef filename;
          // errs() << *II <<"\n";
          // getInstSrcLocation(&(*II), linenum, filename);
          // errs() << "[error]" << filename.str() <<":" <<linenum<<"\n";

          addEdge(Caller, callee, dyn_cast<Instruction>(&*II));
        }
      }
    }
  }

  void destroy(map<Function *, MiniGraphNode *> *graph) {
    map<Function *, MiniGraphNode *>::iterator iter = graph->begin(),
                                               end = graph->end();
    for (; iter != end; iter++) {
      delete iter->second;
    }

    graph->clear();
    delete graph;
  }
};

/*
 * From this structure, we want to figure out which parameters are
 * self-contained within this structure, and those ones have real control
 * dependency with the one under tracking (A in the above example)
 *
 * In the example above, if E is self-contained in the structure, we expect all
 * E's usage is covered by the functions in this structure, i.e., satisfying the
 * following conditions:
 *
 * S = {foo, oof | bar, foobar, rab}
 *
 * 1. all E's usage is within S
 * 2. all bar's usage is within S
 * 3. all rab's usage is within S
 */

#endif // LLVMPASS_MYCALLGRAPH_H
