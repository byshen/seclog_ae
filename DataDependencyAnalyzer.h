// Track data dependency forwards from a given point

#ifndef DATA_DEPENDENCY_ANALYZER
#define DATA_DEPENDENCY_ANALYZER

#include "Common.h"
#include "DataFlowAnalysis.h"
#include "Utils.h"

class TaintInfo : public Info {
public:
  // because the indices and instr are SSA, so they are equivalent
  // a instruction may depend on the value from the set of instructions
  map<unsigned, set<unsigned> *> taintIndices;

  TaintInfo() {}

  ~TaintInfo() {
    explicitReleaseMemory();
    taintIndices.clear();
  }

  void explicitReleaseMemory() {
    // std::cout<<"explicit free memory begins\n";
    for (auto it : taintIndices) {
      if (it.second != nullptr) {
        it.second->clear();
        delete it.second;
      }
    }
    // std::cout<<"explicit free memory ends\n";
  }

  TaintInfo(map<unsigned, set<unsigned> *> indices) { taintIndices = indices; }
  void print() {
    for (auto &idx : taintIndices) {
      errs() << idx.first << "|";
      printSet(idx.second, "\t");
    }
    errs() << '\n';
  }

  void print(std::map<unsigned, Instruction *> IndexToInstr,
             std::map<Instruction *, unsigned> InstrToIndex) {
    for (auto &idx : taintIndices) {
      errs() << *IndexToInstr[idx.first] << "|";
      if (idx.second == nullptr) {
        LOGERR("This instr's set is set to null !!!\n");
        continue;
      }
      for (auto it = idx.second->begin(); it != idx.second->end(); ++it) {
        errs() << *IndexToInstr[*it] << "\t";
      }
      errs() << '\n';
    }
    errs() << '\n';
  }

  map<unsigned, set<unsigned> *> getInfo() { return taintIndices; }

  void setInputInfo(TaintInfo *info) { setInfo(info->taintIndices); }
  void setInfo(map<unsigned, set<unsigned> *> s) {
    // copy into the info with new created set
    for (auto it = s.begin(); it != s.end(); ++it) {
      if (taintIndices.find(it->first) == taintIndices.end())
        taintIndices[it->first] = new set<unsigned>();
      else
        taintIndices[it->first]->clear();
      copySet(taintIndices[it->first], it->second);
    }
  }

  void getImpactedInstIndicies(int idx, set<unsigned> *res) {
    for (auto &it : taintIndices) {
      if (it.second->count(idx) != 0) {
        res->insert(it.first);
      }
    }
  }

  static bool equals(TaintInfo *info1, TaintInfo *info2) {
    auto indices1 = info1->taintIndices;
    auto indices2 = info2->taintIndices;
    set<unsigned> checked;
    for (auto it = indices1.begin(); it != indices1.end(); ++it) {
      if (indices2.count(it->first) == 0)
        return false;
      if (*indices1[it->first] != *indices2[it->first])
        return false;
      checked.insert(it->first);
    }
    // if any elements in 2 is not checked?
    if (checked.size() != indices2.size())
      return false;
    return true;
  }

  static bool contains(TaintInfo *info1, TaintInfo *info2) {
    // if info1 is larger than info 2
    auto indices1 = info1->taintIndices;
    auto indices2 = info2->taintIndices;

    if (indices1.size() < indices2.size())
      return false;

    for (auto it = indices2.begin(); it != indices2.end(); ++it) {
      if (indices1.count(it->first) == 0)
        return false;

      for (auto sitr = it->second->begin(); sitr != it->second->end(); ++sitr) {
        // iterate the set of elements in indicies 2
        if (indices1[it->first]->count(*sitr) == 0) {
          return false;
        }
      }
    }
    return true;
  }

  void insert(unsigned x, unsigned y) {
    // x = y1 + y2
    // tained[x] = {y1, y2} \U tainted[x]
    if (x == 0 || y == 0) {
      assert(false);
    }
    if (taintIndices.find(x) == taintIndices.end()) {
      taintIndices[x] = new set<unsigned>();
    }
    if (taintIndices.find(y) != taintIndices.end()) {
      copySet(taintIndices[x], taintIndices[y]);
    }
    taintIndices[x]->insert(y);
  }

  void replace(unsigned x, unsigned y) {
    // store y, x
    if (x == 0 || y == 0) {
      assert(false);
    }
    if (taintIndices.find(x) == taintIndices.end()) {
      taintIndices[x] = new set<unsigned>();
    } else {
      // clear this set's previous dependencies
      taintIndices[x]->clear();
    }
    if (taintIndices.find(y) != taintIndices.end()) {
      copySet(taintIndices[x], taintIndices[y]);
    }
    taintIndices[x]->insert(y);
  }

  void erase(unsigned x) { taintIndices.erase(x); }

  static bool join(TaintInfo *info1, TaintInfo *info2, TaintInfo *result) {
    // bool flag = TaintInfo::contains(info1, info2);

    // auto indices1 = info1->taintIndices;
    // auto indices2 = info2->taintIndices;
    // for (auto it = indices2.begin(); it!=indices2.end(); it++ ) {
    //     if (indices1.find(it->first) == indices1.end()) { // not found, just
    //     copy
    //         indices1[it->first] = new set<unsigned>();
    //         copySet(indices1[it->first], it->second);
    //     } else { // found it, insert
    //         indices1[it->first]->insert(it->second->begin(),
    //         it->second->end());
    //     }
    // }

    // return flag;

    // // true means result is different
    // // join the two set
    map<unsigned, set<unsigned> *> tmpIndices;
    auto indices1 = info1->taintIndices;
    auto indices2 = info2->taintIndices;
    // copy indices 1
    for (auto it = indices1.begin(); it != indices1.end(); ++it) {
      tmpIndices[it->first] = new set<unsigned>();
      copySet(tmpIndices[it->first], it->second);
    }
    // insert 2 to tmp
    for (auto it = indices2.begin(); it != indices2.end(); it++) {
      if (tmpIndices.find(it->first) ==
          tmpIndices.end()) { // not found, just copy
        tmpIndices[it->first] = new set<unsigned>();
        copySet(tmpIndices[it->first], it->second);
      } else { // found it, insert
        tmpIndices[it->first]->insert(it->second->begin(), it->second->end());
      }
    }
    result->setInfo(tmpIndices);

    // free it
    for (auto &it : tmpIndices) {
      it.second->clear();
      delete it.second;
    }
    tmpIndices.clear();

    if (TaintInfo::equals(info1, result)) {
      return false;
    } else {
      info1->setInfo(result->taintIndices);
      return true;
    }
  }
}; // end of reaching info subclass

// Forward Taint analysis
class TaintAnalysis : public DataFlowAnalysis<TaintInfo, true> {
public:
  ~TaintAnalysis() {
    // clear everything
    destroy();
  }

  typedef std::pair<unsigned, unsigned> Edge;

  void destroy() {
    IndexToInstr.clear();
    InstrToIndex.clear();
    std::cout << "Free memory begin...\n";
    int cnt = 0;
    std::cout << EdgeToInfo.size();
    for (auto it = EdgeToInfo.begin(); it != EdgeToInfo.end(); it++) {
      errs() << "Edge " << it->first.first
             << "->"
                "Edge "
             << it->first.second << ":\n";

      cnt += 1;
      std::cout << "Free memory " << cnt << " taintInfos\n";
      if (it->second != nullptr) {
        std::cout << "Free memory " << cnt << " taintInfos\n";
        delete it->second;
      }
    }
    EdgeToInfo.clear();
    std::cout << "Free memory " << cnt << " taintInfos end...\n";
  }

  // should be called after run worklist!
  void freeAllInfoButLastInfo() {
    // iterate from rbegin+1
    std::cout << "Free memory begin...\n";
    int cnt = 0;
    for (auto it = EdgeToInfo.rbegin(); it != EdgeToInfo.rend(); it++) {
      cnt += 1;
      if (it->second == lastInfo)
        continue;
      it->second->explicitReleaseMemory();
      // delete it->second;
    }

    std::cout << "Free memory " << cnt << " taintInfos end...\n";
  }

  void getImpactedCallInstr(GetElementPtrInst *inst,
                            set<Instruction *> *affectedInstrs) {
    if (inst == nullptr)
      return;
    LOGINFO("Finding impacted call instrs...\n");
    getImpactedInstr(inst, affectedInstrs, CALL_TYPE);
  }

  void getImpactedBranchInstr(Instruction *inst,
                              set<Instruction *> *affectedInstrs) {
    if (inst == nullptr)
      return;
    getImpactedInstr(inst, affectedInstrs, BRANCH_TYPE);
  }

  // should be called after run worklist
  // actually, we only need to keep the last taintInfo :)
  void getImpactedInstr(Instruction *inst, set<Instruction *> *affectedInstrs,
                        ImpactedInstrType instType) {
    if (inst == nullptr)
      return;
    // errs() << *inst <<"\n";
    // const auto it = EdgeToInfo.rbegin();
    // TaintInfo* allInfo = it->second;

    TaintInfo *allInfo = lastInfo;
    // allInfo->print();

    int idx = getInstrToIndex(inst);
    // errs() << idx <<"\n";
    if (idx == -1) {
      LOGERR("The inst is not in the taint analyzed function!\n");
      assert(false);
    }

    set<unsigned> affectedIndices;
    allInfo->getImpactedInstIndicies(idx, &affectedIndices);
    // LOGINFO("all affected indices\n");
    // printSet(&affectedIndices, "\t");

    int cnt = 0;
    for (auto const &it : affectedIndices) {
      Instruction *inst = getIndexToInstr(it);

      if (instType == BRANCH_TYPE) {
        if (isa<BranchInst>(inst)) {
          cnt += 1;
          affectedInstrs->insert(inst);
        }
      } else if (instType == CALL_TYPE) {
        if (isa<CallInst>(inst)) {
          cnt += 1;
          affectedInstrs->insert(inst);
        }
      } else if (instType == ALL_TYPE) {
        cnt += 1;
        affectedInstrs->insert(inst);
      }
    }
    LOGINFO("affected ", cnt, " instrs\n");
    printSetInstr(affectedInstrs, "\n");
  }

  TaintInfo *getLastInfo() {
    const auto it = EdgeToInfo.rbegin();
    errs() << "Edge " << it->first.first
           << "->"
              "Edge "
           << it->first.second << ":\n";
    (it->second)->print(IndexToInstr, InstrToIndex);
    return it->second;
  }

  void flowfunction(Instruction *I, std::vector<unsigned> &IncomingEdges,
                    std::vector<unsigned> &OutgoingEdges, TaintInfo *outInfo) {
    // TaintInfo* outInfo = new TaintInfo();
    auto currIdx = getInstrToIndex(I);
    // errs() << getOpType(I) << "\t" << I->getOpcodeName() << "\t" << currIdx
    // << "\n";;
    for (auto &i : IncomingEdges) {
      // errs() << i << "\t" << currIdx << "\n";
      Edge e = std::make_pair(i, currIdx);
      // getEdgeToInfo(e)->print();
      TaintInfo::join(outInfo, getEdgeToInfo(e), outInfo);
    }
    // errs() << getOpType(I) << "\t" << I->getOpcodeName() << "\n";;

    switch (getInstructionType(I)) {
    case CALL_INST: { // the current idx depend on function parameters
      LOGDEBUG("handle call instr\n");
      CallInst *callInst = dyn_cast<CallInst>(I);
      // to learn about the call function, the callInst->getNumOperands()-1 is
      // the function

      if (callInst->getType()->isVoidTy()) {
        LOGDEBUG("func call with no return ", *callInst, "\n");
        break;
      }
      // This is only for arguments
      for (unsigned i = 0; i < callInst->getNumArgOperands(); ++i) {
        // errs() << i <<"\t arg operand\n"<< *callInst->getArgOperand(i)
        // <<"\n";
        Instruction *tmp = dyn_cast<Instruction>(callInst->getArgOperand(i));
        if (tmp == nullptr)
          continue;
        int idx = getInstrToIndex(tmp);
        // currIdx depend on idx
        if (idx == -1) {
          LOGERR("error in func args\n");
        }
        outInfo->insert(currIdx, idx);
      }

      // we also need to deal with cases that the calling function is
      // function pointer
      // %15 = call i32 %14(%struct.request_rec* %3, i8* getelementptr ...
      Function *func = callInst->getCalledFunction();
      if (func == nullptr) {
        // probably function pointers
        Value *v = callInst->getCalledValue();
        Instruction *tmp = dyn_cast<Instruction>(v);
        if (tmp == nullptr)
          break; // if not able to find a instruction
        int idx = getInstrToIndex(tmp);
        // currIdx depend on idx
        if (idx == -1) {
          LOGERR("error in func args\n");
        }
        outInfo->insert(currIdx, idx);
        // errs()<<*tmp<<"\n";
      }
      break;
    }
    case STORE_INST: {
      LOGDEBUG("handle store instr\n");
      StoreInst *inst = dyn_cast<StoreInst>(I);
      // we don't care about the currIdx itself

      Value *tgt = inst->getOperand(1); // tgt depends on src
      Value *src = inst->getOperand(0);
      if (isa<Instruction>(tgt) && isa<Instruction>(src)) {
        int tgt_idx = getInstrToIndex(dyn_cast<Instruction>(tgt));
        int src_idx = getInstrToIndex(dyn_cast<Instruction>(src));
        if (tgt_idx == -1 || src_idx == -1) {
          LOGERR("operand instruction not found", *src, "\n");
          break;
        }
        /* To improve accuracy, should over-written the tgt's value by src,
         * since all previous definitions are killed now, that could not have
         * impact on.
         */
        // outInfo->insert(tgt_idx, src_idx); // This is IMPRECISE
        // should replace
        outInfo->replace(tgt_idx, src_idx);
      }
      break;
    }
    case LOAD_INST: {
      LOGDEBUG("handle load instr\n");
      LoadInst *inst = dyn_cast<LoadInst>(I);
      Value *src = inst->getOperand(0); // the pointer operand
      LOGDEBUG("src ", *src, "\n");
      if (!isa<Instruction>(src))
        break;
      int idx = getInstrToIndex(dyn_cast<Instruction>(src));
      LOGDEBUG("idx ", idx, "\n");
      /* vsftpd
       * Bug: load a global variable that is not recorded.
       * %16 = load i8*, i8** @do_file_send_rwloop.p_readbuf, align 8
       * load a global variable
       */
      if (idx == -1) {
        LOGERR("operand instruction not found", *src, "\n");
        break;
      }
      outInfo->insert(currIdx, idx);
      break;
    }
    case ICMP_INST: {
      LOGDEBUG("handle icmp instr\n");
      ICmpInst *inst = dyn_cast<ICmpInst>(I);
      for (unsigned i = 0; i < inst->getNumOperands(); ++i) {
        if (isa<Instruction>(inst->getOperand(i))) {
          int idx = getInstrToIndex(dyn_cast<Instruction>(inst->getOperand(i)));
          if (idx == -1) {
            LOGERR("operand instruction not found", *inst->getOperand(i), "\n");
            continue;
          }
          outInfo->insert(currIdx, idx);
        } else {
          LOGDEBUG("icmp with operads", *inst->getOperand(i), "\n");
        }
      }
      break;
    }
    case BR_INST: { // Terminator
      // actuall we care about branch conditions, but only for condition itself
      // Then we simply scan the branch conditions that contain the point of
      // interest.
      // TODO: the next two BB depends on the instruction, we may
      //       add the branch's impact (which may cause more imprecision)
      LOGDEBUG("handle branch instr\n");
      BranchInst *inst = dyn_cast<BranchInst>(I);
      if (inst->isUnconditional()) { // jump instr has 1 operand
        break;
      }
      if (!isa<Instruction>(inst->getCondition())) {
        break;
      }
      int idx = getInstrToIndex(dyn_cast<Instruction>(inst->getCondition()));
      if (idx == -1) {
        LOGERR("operand instruction not found", *inst->getCondition(), "\n");
        break;
      }
      LOGDEBUG("condition is idx", idx, "\n");
      outInfo->insert(currIdx, idx);
      break;
    }
    case BINARY_OP_INST:
    case GETELEMENTPTR_INST:
      // we don't really care about the concrete value, just need to know which
      // value it depends on, such as if the address/value is dependent on x

      // GetElementPtrInst* inst = dyn_cast<GetElementPtrInst>(I);
    case CAST_INST:
      // bitcast/truc/sext/zext, unary op
    default:
      LOGDEBUG("handled with debug ", *I, "\n");
      if (I->getType()->isVoidTy()) {
        LOGDEBUG("skip if no return\n");
        break;
      }
      for (unsigned i = 0; i < ((User *)I)->getNumOperands(); ++i) {
        if (!isa<Instruction>(I->getOperand(i)))
          break;
        int idx = getInstrToIndex((Instruction *)I->getOperand(i));
        if (idx == -1) {
          LOGERR("operand", *I->getOperand(i), " is not instr\n");
          continue;
        }
        outInfo->insert(currIdx, idx);
      }
      break;
    }

    // for (size_t i =0;i<OutgoingEdges.size(); ++i) {
    //     Infos.push_back(outInfo);
    // }
    return;
  }

public:
  TaintAnalysis(TaintInfo &bottom, TaintInfo &rInfo)
      : DataFlowAnalysis(bottom, rInfo) {}
}; // end of subclass DFA

// Store all info related to the instruction, namely the return value of the
// instruction if the return value of the function is used, its DependInfo would
// be nullptr
struct DependInfo {
  set<Value *> taintedValues;

  void insert(Value *v) { taintedValues.insert(v); }

  bool exist(Value *v) { return taintedValues.find(v) != taintedValues.end(); }
};

void trackDataFlowDepend(Value *v, DependInfo *dependInfo) {
  if (v == nullptr) {
    errs() << "[error] v is null in trackDataFlowDepend";
    return;
  }

  for (auto I = v->use_begin(); I != v->use_end(); I++) {
    if (dependInfo->exist(*I))
      continue;
    dependInfo->insert(*I);

    if (isa<Function>(*I)) {
      Function *func = dyn_cast<Function>(*I);
      dbgs() << "[info]" << *func << "\t" << func->getName();
    }

    if (isa<Instruction>(*I)) {
      Instruction *useInst = dyn_cast<Instruction>(*I);
      dbgs() << "[info] Get instruction" << *useInst << "\n";
    } else {
      dbgs() << "[error]" << *I << "\n";
      assert(false);
    }

    switch (getInstructionType(*I)) {
    case CALL_INST: {
      // should always hit here because it started with the call sites
      dbgs() << "[info] Get CALL INST"
             << "\n";

      CallInst *callInst = dyn_cast<CallInst>(*I);

      trackDataFlowDepend(callInst, dependInfo);
      break;
    }
    case TODO_INST: {
      dbgs() << "[TODO] instruction type not handled for now";
      break;
    }
    default:
      break;
    }
  }
}

#endif