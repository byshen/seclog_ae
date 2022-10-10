//
// July 11 2020
#ifndef _DepGraphs_H_
#define _DepGraphs_H_

#include "Common.h"
#include "DGFuncAnalyzer.h"
#include "DataDependencyAnalyzer.h" // TaintAnalysis
#include "DataFlowAnalysis.h"
#include "LogicRelationAnalysis.h"
#include "Struct.h"
#include "Utils.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include <algorithm>

// only for backward slicing;
// dependece graph for one procedure

class DepGraph {
public:
  Module *_module;
  Function *_func;

  map<Instruction *, int> InstrToIdx;
  map<int, Instruction *> IdxToInstr;

  map<Value *, int> GVToIdx;
  map<int, Value *> IdxToGV;

  map<int, set<int> *> controlDepGraph;
  map<int, set<int> *> dataDepGraph;

  map<int, set<int> *> pdg; // procedure dependence graph
  map<Instruction *, set<int>> sliceResultCache;

  map<Instruction *, set<Instruction *> *>
      mpStructToGeps; // struct to all uses of the struct
  map<Instruction *, pair<Instruction *, int>>
      fieldDepPairs; // map gep, to the field of the struct

  bool isFieldSensitive = false;
  void printInstrs() {
    errs() << "===========instruction id <-> instr\n";

    for (auto &i : IdxToInstr) {
      errs() << i.first << "\t" << *i.second << "\n";
    }
  }

  DepGraph(Module *_m, Function *_f, bool isFieldS = false) {
    if (_m == nullptr || _f == nullptr) {
      LOGERR("WRONG INIT IN DEPGRAPH!\n");
      assert(false);
    }

    _module = _m;
    _func = _f;

    isFieldSensitive = isFieldS;

    // assign the initial idx
    int id = 1;

    // global vars
    for (auto I = _module->global_begin(); I != _module->global_end(); I++) {
      GlobalVariable *gv = dyn_cast<GlobalVariable>(I);
      GVToIdx[gv] = id;
      IdxToGV[id] = gv;
      id++;
    }

    // function _f
    for (inst_iterator I = inst_begin(_f), IE = inst_end(_f); I != IE; I++) {
      Instruction *inst = &*I;
      InstrToIdx[inst] = id;
      IdxToInstr[id] = inst;
      id++;

      // for field sensitive ones only;
      if (isa<GetElementPtrInst>(inst)) {
        processGEPInstr(inst, mpStructToGeps, fieldDepPairs);
      }
    }

    errs() << "Init DepGraph Ends with " << IdxToInstr.size()
           << " instructions\n";
#ifdef LOG_LEVEL_DEBUG
    printInstrs();
#endif
  }

  ~DepGraph() {
    for (auto &s : controlDepGraph) {
      delete s.second;
    }
    for (auto &s : dataDepGraph) {
      delete s.second;
    }
  }

  void updateGraph(map<int, set<int> *> &mp, int src, int tgt) {
    // errs() << "Updating graph\t" << src << "\t" << tgt <<"\n";
    if (mp.find(src) == mp.end()) {
      mp[src] = new set<int>();
      mp[src]->insert(tgt);
    } else {
      mp[src]->insert(tgt);
    }

    if (mp.find(tgt) == mp.end()) {
      mp[tgt] = new set<int>(); // for alloca instrs :)
    }
  }

  void mergeGraph() {
    // from the control dep graph and data dep graph;
    // simply merge based on the key of the graph
    for (auto &it : controlDepGraph) {
      pdg[it.first] = new set<int>();
      pdg[it.first]->insert(controlDepGraph[it.first]->begin(),
                            controlDepGraph[it.first]->end());
    }
    for (auto &it : dataDepGraph) {
      if (pdg.find(it.first) == pdg.end()) {
        pdg[it.first] = new set<int>();
        pdg[it.first]->insert(dataDepGraph[it.first]->begin(),
                              dataDepGraph[it.first]->end());
      } else {
        pdg[it.first]->insert(dataDepGraph[it.first]->begin(),
                              dataDepGraph[it.first]->end());
      }
    }
  }

  int getValueIdx(Value *v) {
    int tgtInt = -1;
    if (Instruction *tmp = dyn_cast<Instruction>(v)) {
      tgtInt = InstrToIdx[tmp];
    } else {
      // if v is not a Instruction, we try to see if it is
      // a global variable :)
      if (GlobalVariable *G = dyn_cast<GlobalVariable>(v)) {
        tgtInt = GVToIdx[G];
      }
    }
    return tgtInt;
  }
  void buildControlDepGraph() {
    // only care about the BR instructions with two successors
    for (inst_iterator IB = inst_begin(_func), IE = inst_end(_func); IB != IE;
         IB++) {
      Instruction *I = &*IB;
      int tgtInt = InstrToIdx[I];

      // construct control dep graph only realte to br instrutions.
      //
      // TODO: handle fake dependencies?
      if (isa<BranchInst>(I)) {
        BranchInst *brinst = dyn_cast<BranchInst>(I);
        if (brinst->isConditional()) { // jump instr has 1 operand
          for (unsigned it = 0; it < brinst->getNumSuccessors(); ++it) {
            BasicBlock *bb = brinst->getSuccessor(it);
            for (auto iib = bb->begin(), iie = bb->end(); iib != iie; iib++) {
              Instruction *tmp = &*iib;
              int srcInt = InstrToIdx[tmp];
              updateGraph(controlDepGraph, srcInt, tgtInt);
            }

            // if this bb only has one successor, we also need to connect it
            while (bb->getUniqueSuccessor()) {
              bb = bb->getUniqueSuccessor();
              for (auto iib = bb->begin(), iie = bb->end(); iib != iie; iib++) {
                Instruction *tmp = &*iib;
                int srcInt = InstrToIdx[tmp];
                updateGraph(controlDepGraph, srcInt, tgtInt);
              }
            }
          }
        } else {
          continue;
        }
      }
    }
  }

  void buildDataDepGraph() {
    // TODO: may include pointer analysis later for higher precision
    for (inst_iterator IB = inst_begin(_func), IE = inst_end(_func); IB != IE;
         IB++) {
      Instruction *I = &*IB;
      int srcInt = InstrToIdx[I];

      // we do not consider unconditional jump
      if (isa<BranchInst>(I)) {
        BranchInst *brinst = dyn_cast<BranchInst>(I);
        if (brinst->isUnconditional()) {
          continue;
        }
      }
      if (isa<StoreInst>(I)) {
        StoreInst *inst = dyn_cast<StoreInst>(I);
        // we don't care about the currIdx itself
        Value *tgt = inst->getOperand(1); // tgt depends on src
        Value *src = inst->getOperand(0);
        if (isa<Instruction>(tgt) && isa<Instruction>(src)) {
          Instruction *tgtInst = dyn_cast<Instruction>(tgt);
          Instruction *srcInst = dyn_cast<Instruction>(src);
          updateGraph(dataDepGraph, InstrToIdx[tgtInst], InstrToIdx[srcInst]);
          // errs() << "Two instrs it in store inst " <<*I<<"\n";

          // when a store happens, if we modify a field in the
          // struct, we add the dependecy to the original struct too
          if (isFieldSensitive) {
            if (isa<GetElementPtrInst>(tgtInst)) {
              if (fieldDepPairs.find(tgtInst) != fieldDepPairs.end()) {
                // add true depdencies here
                updateGraph(dataDepGraph,
                            InstrToIdx[fieldDepPairs[tgtInst].first],
                            InstrToIdx[srcInst]);
              }
            }
          }

        } else {
          errs() << "Not two instruction!!! in store instr" << *I << "\n";
          errs() << "src\t " << *src << "\n";
          errs() << "tgt\t " << *tgt << "\n";
          if (isa<ConstantExpr>(src) && isa<Instruction>(tgt)) {
            Instruction *tgtInst = dyn_cast<Instruction>(tgt);

            errs() << "Global var in store instr" << *I << "\n";
            ConstantExpr *CE = dyn_cast<ConstantExpr>(src);

            for (unsigned i = 0, e = CE->getNumOperands(); i != e; ++i) {
              if (GlobalVariable *srcOp =
                      dyn_cast<GlobalVariable>(CE->getOperand(i))) {
                updateGraph(dataDepGraph, InstrToIdx[tgtInst], GVToIdx[srcOp]);
                // the tgt is part of a struct
                if (isFieldSensitive) {
                  if (isa<GetElementPtrInst>(tgtInst)) {
                    if (fieldDepPairs.find(tgtInst) != fieldDepPairs.end()) {
                      // add true depdencies here
                      updateGraph(dataDepGraph,
                                  InstrToIdx[fieldDepPairs[tgtInst].first],
                                  GVToIdx[srcOp]);
                    }
                  }
                }
              }
            }
          }
        }
      }

      // if it is a callinstruction, also find if there is dependecy
      // between the parameters, can be achieved by an analysis on the called
      // function
      // %14 = call i32 @assign(i32* %2, i32* %local_b), !dbg !142
      if (isa<CallInst>(I)) {
        // if call an library call function, simply assume the pointer values
        // depend on other values;
        list<Value *> pointerValueParams;
        list<Value *> allParams;
        for (Use &U : I->operands()) {
          Value *v = U.get();
          allParams.push_back(v);
          if (v->getType()->isPointerTy()) {
            pointerValueParams.push_back(v);
          }
        }

        for (Value *ptrValue : pointerValueParams) {
          int tgtInt = getValueIdx(ptrValue);
          if (tgtInt == -1) {
            continue;
          }

          for (Value *otherValue : allParams) {
            if (otherValue != ptrValue) {
              int srcInt = getValueIdx(otherValue);
              if (srcInt == -1) {
                continue;
              }
              errs() << "DATA DEP BETWEEN PARAMS " << srcInt << "\t " << tgtInt
                     << "\n";
              updateGraph(dataDepGraph, srcInt, tgtInt);
            }
          }
        }
      }

      for (Use &U : I->operands()) {
        Value *v = U.get();
        // if (Instruction* tmp = dyn_cast<Instruction>(v)) {
        //     int tgtInt = InstrToIdx[tmp];
        //     updateGraph(dataDepGraph, srcInt, tgtInt);
        // }
        // else {
        //     // if v is not a Instruction, we try to see if it is
        //     // a global variable :)
        //     if (GlobalVariable* G = dyn_cast<GlobalVariable>(v)) {
        //         int tgtInt = GVToIdx[G];
        //         updateGraph(dataDepGraph, srcInt, tgtInt);
        //     }
        // }
        int tgtInt = getValueIdx(v);
        if (tgtInt != -1) {
          updateGraph(dataDepGraph, srcInt, tgtInt);
        }
      }
    }
  }

  void setDataDepGraphAsPDG() {
    pdg.clear();
    for (auto &it : dataDepGraph) {
      pdg[it.first] = new set<int>();
      pdg[it.first]->insert(dataDepGraph[it.first]->begin(),
                            dataDepGraph[it.first]->end());
    }
    // print(pdg);
  }

  void buildOnlyDataDepGraph() {
    buildDataDepGraph();
    setDataDepGraphAsPDG();
  }

  void buildGraph() {
    buildControlDepGraph();
    // print(controlDepGraph);

    buildDataDepGraph();
    // print(dataDepGraph);

    mergeGraph();
    // print(pdg);

    // test();
  }

  void test() {
    set<int> res;
    set<int> visited;
    dfs(15, res, visited);
    printSet(&res, "\t");
  }
  void print(map<int, set<int> *> mp) {
    errs() << "Dependence graph of " << _func->getName() << "\n";
    for (auto &idx : mp) {
      errs() << idx.first << "|";
      printSet(idx.second, "\t");
    }
    errs() << '\n';
  }

  void getSlicedArgs(Instruction *src, set<ParamPos> &result,
                     set<Value *> &GVResult) {
    // In this version, we get the arugments
    // let's try with the alloca instruction, which has the debug values;

    // (request *a, int b)
    // { a->0, b->1}
    // we only need the relative position, since we only need to print the
    // variables in the original context :(

    set<Value *> args;
    map<Value *, int> argPosition;

    // This maps the arguments to a specific alloca instruction
    map<Value *, int> argIntMap;

    int pos = 0;
    for (auto ab = _func->arg_begin(), ae = _func->arg_end(); ab != ae; ++ab) {
      auto abb = &*ab;
      Value *a = dyn_cast<Value>(abb);
      // Instruction * vv = (Instruction *) a;
      args.insert(a);

      argPosition[a] = pos;
      pos++;
    }

    // This relies on [-g -O0] optimization
    // bug: %35 = zext i1 %6 to i8
    //      store i8 %35, i8* %14, align 1
    //   %6 is the parameter
    map<Value *, Value *> tmpCastArgs;
    for (inst_iterator I = inst_begin(_func), IE = inst_end(_func); I != IE;
         I++) {
      Instruction *inst = &*I;
      if (isa<StoreInst>(inst)) {
        Value *a = inst->getOperand(0); // store source
        if (args.find(a) != args.end()) {
          // should be the first value initialization :)
          if (argIntMap.find(a) == argIntMap.end()) {
            Instruction *ars = dyn_cast<Instruction>(I->getOperand(1));
            if (ars != nullptr) {
              argIntMap[a] = InstrToIdx[ars];
              // argIntMap[I->getOperand(1)] = InstrToIdx[ars];
            }
          }
        }
      } else { // we do the ugly hack for the above example
        if (isa<CastInst>(inst)) {
          Value *a = inst->getOperand(0);
          if (args.find(a) != args.end()) {
            tmpCastArgs[inst] = a;
          }
        }
      }
    }
    errs() << "TMP CAST ARGS!!" << tmpCastArgs.size() << "\n";

    if (tmpCastArgs.size() != 0) {
      for (inst_iterator I = inst_begin(_func), IE = inst_end(_func); I != IE;
           I++) {
        Instruction *inst = &*I;
        if (isa<StoreInst>(inst)) {
          Value *a = inst->getOperand(0); // store source
          if (tmpCastArgs.find(a) != tmpCastArgs.end()) {
            errs() << "TMP CAST ARGS!!" << *inst << *a << "\n";
            Instruction *ars = dyn_cast<Instruction>(I->getOperand(1));
            if (ars != nullptr) {
              // do the cast here :)
              argIntMap[tmpCastArgs[a]] = InstrToIdx[ars];
            }
          }
        }
      }
    }

    // BUG
    errs() << "TMP debug argintmap!!" << argIntMap.size() << "\t" << args.size()
           << "\n";

    assert(argIntMap.size() == args.size());

    set<int> sliceSet = slice(src);

    // set<int> res;
    // set<int> visited;
    // dfs(15, res, visited);
    // printSet(&res, "\t");
    // set<int>  sliceSet = res;

    // if any args is in the slice
    for (auto &it : argIntMap) {
      if (sliceSet.find(it.second) != sliceSet.end()) {
        Type *t = it.first->getType();
        Type *elemt = isa<PointerType>(t)
                          ? dyn_cast<PointerType>(t)->getElementType()
                          : t;
        LOGINFO(*it.first, "\t", *it.first->getType(), "\t",
                isa<StructType>(elemt), "\n");

        if (isFieldSensitive && isa<StructType>(elemt)) { // if it is a struct
          LOGERR("Processing struct argument ", *it.first, "\n");
          auto allocaInst = IdxToInstr[it.second]; // the alloca instruction
          LOGERR("Corresponding alloca instr ", *it.first, "\n");
          // then we find it in the mpStructToGeps
          if (mpStructToGeps.find(allocaInst) != mpStructToGeps.end()) {
            // the allocaInst is ever been used in a getelementptr instr
            auto setptr = mpStructToGeps[allocaInst];
            for (auto setitr = setptr->begin(); setitr != setptr->end();
                 setitr++) {
              Instruction *geteleInstr = *setitr;
              // if the geteleleInstr is used in the slice, we add it to the set
              if (sliceSet.find(InstrToIdx[geteleInstr]) != sliceSet.end()) {
                result.insert(make_pair(argPosition[it.first],
                                        fieldDepPairs[geteleInstr].second));
              }
            }
          } else { //
            result.insert(make_pair(argPosition[it.first],
                                    -2)); // no field of the struct is used :(
          }
        } else {
          result.insert(make_pair(argPosition[it.first], -1));
        }
      }
    }

    // insert the global values;
    for (auto i : sliceSet) {
      if (IdxToGV.find(i) != IdxToGV.end()) {
        errs() << "GV in AC func" << *IdxToGV[i] << "\n";
        GVResult.insert(IdxToGV[i]);
      }
    }
  }

  set<int> slice(Instruction *src) {
    // return the set of instructions based on
    // the source

    if (sliceResultCache.find(src) != sliceResultCache.end()) {
      // already sliced before ;)
      return sliceResultCache[src];
    }
    set<int> res;
    if (InstrToIdx.find(src) == InstrToIdx.end())
      assert("wrong instruction for the slice!" && false);

    int entry = InstrToIdx[src];
    set<int> visited;
    dfs(entry, res, visited);

    errs() << "[slice result]\t" << entry << "\n";
    printSet(&res, "\t");

    sliceResultCache[src] = res; // is a copy
    return res;
  }

  // a mix of GV and local instrs
  set<Value *> sliceInstrs(Instruction *src) {
    // return the set of instructions based on
    // the source
    set<int> res = slice(src);
    set<Value *> resInstr;
    for (auto &i : res) {
      if (IdxToGV.find(i) != IdxToGV.end())
        resInstr.insert(IdxToGV[i]);
      if (IdxToInstr.find(i) != IdxToInstr.end())
        resInstr.insert(IdxToInstr[i]);
    }
    return resInstr;
  }

  void dfs(int st, set<int> &res, set<int> &visited) {
    if (visited.find(st) != visited.end())
      return;

    visited.insert(st);
    res.insert(pdg[st]->begin(), pdg[st]->end());
    for (auto itr = pdg[st]->begin(); itr != pdg[st]->end(); ++itr) {
      dfs(*itr, res, visited);
    }
  }
};
#endif // DepGraph