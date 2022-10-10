// Track data dependency forwards from a given point

#ifndef LogicRelation_ANALYZER
#define LogicRelation_ANALYZER

#include "Common.h"
#include "DataFlowAnalysis.h"
#include "ResultCheckFunc.h"
#include "Utils.h"
#include "llvm/IR/IntrinsicInst.h"

class LogicInfo : public Info {
public:
  // because the indices and instr are SSA, so they are equivalent
  // a instruction may depend on the value from the set of instructions
  map<unsigned, set<unsigned> *> taintIndices;

  LogicInfo() {}

  ~LogicInfo() {
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

  LogicInfo(map<unsigned, set<unsigned> *> indices) { taintIndices = indices; }
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
  void setInputInfo(LogicInfo *info) {
    if (info == nullptr) {
      LOGERR("canot set null info\n");
      return;
    }
    setInfo(info->taintIndices);
  }

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

  static bool equals(LogicInfo *info1, LogicInfo *info2) {
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

  static bool contains(LogicInfo *info1, LogicInfo *info2) {
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
      delete taintIndices[x];
      taintIndices[x] = new set<unsigned>();
    }
    if (taintIndices.find(y) != taintIndices.end()) {
      copySet(taintIndices[x], taintIndices[y]);
    }
    taintIndices[x]->insert(y);
  }

  void erase(unsigned x) { taintIndices.erase(x); }

  static bool join(LogicInfo *info1, LogicInfo *info2, LogicInfo *result) {
    // // true means result is different
    // // join the two set
    // info2 is always larger than info1
    // if store happens, info2 should be the true ones, abandon previous
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

    if (LogicInfo::equals(info1, result)) {
      return false;
    } else {
      // info1->setInfo(result->taintIndices);

      // This line worths 3 hours of debugging :)
      // BUG: this may cause the lattice not going up, since info2 is not always
      // larger than info1, cause the result to be not deterministic :(
      info1->setInfo(info2->taintIndices);
      return true;
    }
  }
}; // end of reaching info subclass

enum LogicType {
  EQUIV, // equivilant target
  ADD,
  CALL,
  ICMP,
  BR,   // for root only
  LEAF, // Leave as comparison type for leaf!
  GEPTR,
};

struct LogicOps {
  Instruction *val; // this instruction
  Instruction *op1; // which instruction
  Instruction *op2; // we support at most 2 instruction
  LogicType opType;
  LogicOps *lop1;
  LogicOps *lop2;
  LogicOps *parent;

  CmpInst::Predicate pred; // only for icmp
  int cmpInt;              // only for icmp

  int gepField;               // only for getElementPtr instruction

  LogicOps(Instruction *v, Instruction *i1, LogicType ty,
           Instruction *i2 = nullptr) {
    // may only have one such as store/unaryOp
    val = v;
    op1 = i1;
    opType = ty;
    op2 = i2;
  }

  // only need this special to handle cases that
  bool isEquiv() { return opType == EQUIV; }
  bool isLeaf() { return opType == LEAF; }

  // used in icmp, the only child is a leaf node
  bool isChildLeaf() {
    if (lop1 == nullptr || lop2 != nullptr)
      return false;
    return lop1->isLeaf();
  }
  /* @retval : 0 - true label
   *           1 - false label
   */
  int matchDeniedBranch(ResultCheckFunc *rcf) {
    // currently more like hard coded, needs
    // a sound algorithm for that
    if (opType != ICMP) {
      LOGERR("Should not get denied branch from non-icmp instruction\n");
      return -1;
    }
    ICmpInst *inst = dyn_cast<ICmpInst>(val);
    if (rcf->type == ResultCheckFuncType::RESULT_NE) {
      if (inst->getPredicate() == CmpInst::ICMP_EQ)
        return 1;
      else if (inst->getPredicate() == CmpInst::ICMP_NE) {
        return 0;
      }
    }
    if (rcf->type == ResultCheckFuncType::RESULT_EQ) {
      if (inst->getPredicate() == CmpInst::ICMP_EQ)
        return 0;
      else if (inst->getPredicate() == CmpInst::ICMP_NE) {
        return 1;
      }
    }
    if (rcf->type == ResultCheckFuncType::RESULT_LT) {
      if (inst->getPredicate() == CmpInst::ICMP_SLT ||
          inst->getPredicate() == CmpInst::ICMP_ULT)
        return 0;
      else if (inst->getPredicate() == CmpInst::ICMP_SGT ||
               inst->getPredicate() == CmpInst::ICMP_UGT) {
        return 1;
      }
    }
    if (rcf->type == ResultCheckFuncType::RESULT_GT) {
      if (inst->getPredicate() == CmpInst::ICMP_SGT ||
          inst->getPredicate() == CmpInst::ICMP_UGT)
        return 0;
      else if (inst->getPredicate() == CmpInst::ICMP_SLT ||
               inst->getPredicate() == CmpInst::ICMP_ULT) {
        return 1;
      }
    }

    // TODO: Need to implement for other types!
    if (rcf->type == ResultCheckFuncType::RESULT_GE ||
        rcf->type == ResultCheckFuncType::RESULT_LE) {
      LOGERR("not supported RCF");
      assert(false);
      return -1;
    }
    return -1; // Fix annoying compiler warnings
  }

  void printString() {
    errs() << *val << "\t->\t";
    if (op1)
      errs() << *op1 << " ";

    string opstr;
    switch (opType) {
    case EQUIV:
      opstr = "[equiv]";
      break;
    case ICMP:
      // need predicate as well, later
      if (pred == CmpInst::Predicate::ICMP_EQ)
        opstr = "[icmp_eq]";
      else if (pred == CmpInst::Predicate::ICMP_NE)
        opstr = "[icmp_ne]";
      opstr += " " + to_string(cmpInt);
      break;
    case ADD:
      opstr = "+";
      break;
    case LEAF:
      opstr = "[LEAF]";
      break;
    case CALL:
      opstr = "[call]";
      break;
    default:
      break;
    }
    errs() << opstr;
  }
};

// Forward Taint analysis
class LogicAnalysis : public DataFlowAnalysis<LogicInfo, true> {
public:
  Instruction *tgtInstr;
  Instruction *tgtBrInstr;
  set<unsigned> tgtAffectedInstrs;

  // store its relationship with tgtInstr
  // (e.g. store %10, %4, create entry %4 = LogicOps(%10, EQUIV) )
  map<Instruction *, LogicOps *> instCache;
  // store the operation tree head
  // build from instCache
  LogicOps *opTreeNode;
  // mark as leaf
  LogicOps *leafNode;

  typedef std::pair<unsigned, unsigned> Edge;

  // fields
  // collected while construct CFG
  // only used in access check function analysis
  // %16 = getelementptr inbounds %struct.request, %struct.request* %15, i32 0,
  // i32 0
  map<Instruction *, pair<Instruction *, int>> fieldDepPairs;

  map<Instruction *, pair<Instruction *, int>> *getFieldDepPairs() {
    return &fieldDepPairs;
  }
  ~LogicAnalysis() {
    // clear everything
    destroy();
  }

  void setAnalysisTarget(Instruction *instr) { tgtInstr = instr; }
  void setAnalysisBrTarget(Instruction *instr) { tgtBrInstr = instr; }

  void printCache() {
    if (instCache.size() == 0)
      LOGERR("no cache found!\n");
    for (auto caItem : instCache) {
      caItem.second->printString();
      errs() << "\n";
    }
  }

  void printBackEdges() {
    for (auto item : backEdges) {
      errs() << item.first << "\t->\t";
      printSet(&item.second, "\t");
    }
  }

  bool foundTgtInstrInBackTrack = false;
  int searchDeniedBranch(Instruction *tInstr, Instruction *tbInstr,
                         ResultCheckFunc *rcf) {
    LOGINFO("===========search for denied branch begins\n");
    // clear tmp tates
    _visited.clear();
    foundTgtInstrInBackTrack = false;

    // set up for analysis
    setAnalysisTarget(tInstr);
    setAnalysisBrTarget(tbInstr);
    getImpactedInstrIndex(tgtInstr, &tgtAffectedInstrs, ALL_TYPE);
    // printBackEdges();
    // begin tracking
    recordLogicOps(tgtBrInstr);
    if (foundTgtInstrInBackTrack == false) {
      LOGERR("Could not find the target instr!\n");
      return -1;
    }

    printCache();
    leafNode = new LogicOps(tgtInstr, nullptr, LEAF);
    // set<Instruction*> treeVisited;

    instCache[tgtBrInstr]->printString();

    opTreeNode = buildOpsTree(tgtBrInstr);
    reduceTree(opTreeNode);
    // TODO: BUG should change the original op operand!!
    if (opTreeNode == nullptr) {
      LOGERR("Failed to build tree!\n");
    }

    // return null if does not match
    int deniedBranch = getDeniedBranchFromTree(opTreeNode, rcf);
    if (deniedBranch != -1) {
      BranchInst *inst = dyn_cast<BranchInst>(opTreeNode->val);
      errs() << *(inst->getSuccessor(deniedBranch));
      return deniedBranch;
    } else {
      LOGERR("Denied branch not found!\n");
      return -1;
    }

    // printSet(&tgtAffectedInstrs, "\n");
    LOGINFO("===========search for denied branch ends\n");
  }

  /* retval:
      -1 Not found
      0 choose true branch
      1 choose false branch
  */
  int getDeniedBranchFromTree(LogicOps *tree, ResultCheckFunc *rcf) {
    if (!isa<BranchInst>(tree->val)) {
      LOGERR("Tree head should be branch inst!\n");
      return -1;
    }
    if (rcf->isBoolean()) {
      // Case 0.0, C++ only, boolean value in the br instruction
      //  %40 = call zeroext i1 @pg_class_ownercheck(i32 %38, i32 %39)
      //  br i1 %40, label %48, label %41
      if (tree->lop1->isLeaf()) {
        // leaf is already the target leaf
        int whichBranch;
        if (rcf->chooseFalse())
          whichBranch = 1;
        else if (rcf->chooseTrue())
          whichBranch = 0;
        if (rcf->type == ResultCheckFuncType::RESULT_NE) {
          whichBranch = 1 - whichBranch; // do a conversion.
        }
        LOGINFO("Match choose ", whichBranch, "\n");
        // BranchInst* inst = dyn_cast<BranchInst>(tree->val);
        return whichBranch;
      } else {
        LOGERR("Unknown boolean pattern!\n");
        return -1;
      }
    } else if (rcf->isEquiv() ||
               rcf->type ==
                   ResultCheckFuncType::RESULT_LT) { // The most simple case
      // if it is eq type, simply find the branch value match or not
      if (tree->lop1 == nullptr) {
        LOGERR("Root BR hould have lop1!\n");
        return -1;
      }
      // Case 1.0:
      // the condition in icmp is the leaf
      if (tree->lop1->opType == LogicType::ICMP && tree->lop1->isChildLeaf() &&
          tree->lop1->cmpInt == rcf->denyVal) {
        LOGINFO("Match with cmpInt in icmp\n");
        int whichBranch = tree->lop1->matchDeniedBranch(rcf);
        if (whichBranch == -1) {
          return -1;
        }
        LOGINFO("Match choose ", whichBranch, "\n");
        // BranchInst* inst = dyn_cast<BranchInst>(tree->val);
        return whichBranch;
      }
      // Case 2.0: handle case that the result check function will
      // call some functions to judge whether this result is denied or not
      // e.g. %71 = call i32 @vsf_sysutil_retval_is_error(i32 %70)
      //      %72 = icmp ne i32 %71, 0
      //      br i1 %72, label %74, label %73
      else if (tree->lop1->opType == LogicType::ICMP &&
               rcf->hasValCheckFunc() &&
               // tree->lop1->opType == LogicType::CALL &&
               tree->lop1->cmpInt == rcf->denyVal) {
        LOGDEBUG("match with call function in icmp\n");
        // do more checking
        // tree->lop1 = icmp
        // tree->lop1->lop1 should be the valcheckfunc

        if (isa<CallInst>(tree->lop1->op1)) {
          CallInst *callinst = dyn_cast<CallInst>(tree->lop1->op1);
          Function *tmp = callinst->getCalledFunction();
          LOGDEBUG(tmp->getName(), "\n");
          if (tmp != rcf->valCheckFunc) {
            return -1;
          }
          // now check call function's arg is leaf or not
          if (!tree->lop1->lop1->isChildLeaf()) {
            return -1;
          }

          LOGINFO("wow, match the val check func\n");
          int whichBranch = tree->lop1->matchDeniedBranch(rcf);
          if (whichBranch == -1) {
            return -1;
          }
          LOGINFO("Match choose ", whichBranch, "\n");
          // BranchInst* inst = dyn_cast<BranchInst>(tree->val);
          return whichBranch;
        }
      } 
      else if (rcf->hasField()){
        // handle the case
        LOGDEBUG("try to match return value with a field\n");
        if (tree->lop1->opType == LogicType::ICMP) {
          LogicOps* child = tree->lop1->lop1;
          // SKIP EQUIV TYPES
          while (child->isEquiv()) { 
            child = child->lop1;
          }
          if (child->opType == LogicType::GEPTR) {
            if (child->gepField == rcf->field) {
              LOGDEBUG("FIELD MATCH!!");
              int whichBranch = tree->lop1->matchDeniedBranch(rcf);
              if (whichBranch == -1) {
                return -1;
              }
              LOGINFO("Match choose ", whichBranch, "\n");
              return whichBranch;
            }
          }
        }
      }
      LOGERR("May meet the wrong cmpInt in rcf", rcf->denyVal, "\n");
      if (tree->lop1 != nullptr && tree->lop1->opType == LogicType::ICMP) {
        LOGERR("May meet the wrong cmpInt in lop1", tree->lop1->cmpInt, "\n");
      }
    }
    LOGERR("not handled pattern in finding denied branch!\n");
    return -1;
  }

  LogicOps *buildOpsTree(Instruction *inst) {
    // TODO: how to avoid circular structure?
    if (inst == nullptr)
      return nullptr;
    LOGDEBUG("BUILD TREE ", *inst, "\n");
    if (instCache.count(inst) == 0)
      return nullptr;
    if (instCache[inst] == nullptr)
      return nullptr;

    if (inst == tgtInstr) {
      LOGINFO("Meet the target instr in building tree!\n");
    }

    // if (instCache[inst]->isEquiv()) {
    //     LogicOps* realNode = buildOpsTree(instCache[inst]->op1);
    //     return realNode;
    // }

    instCache[inst]->lop1 = buildOpsTree(instCache[inst]->op1);
    instCache[inst]->lop2 = buildOpsTree(instCache[inst]->op2);
    return instCache[inst];
  }

  // based on the target branch instruction,
  // and the original instr, to the extent possible that
  // the branch instruction can be expressed by the original instr.
  // then compare the tree with resultCheckFunction
  Instruction *reduceTree(LogicOps *&curr) {
    if (curr == nullptr) {
      return nullptr;
    }
    LOGDEBUG("REDUCE TREE ", *curr->val, "\n");
    if (curr->isLeaf()) {
      LOGDEBUG("Meet leaf!\n");
      if (instCache[curr->val]->isLeaf()) {
        // LOGDEBUG("Also meet leaf in instCache !\n");
      }
      return curr->val;
    }
    if (curr->isEquiv()) {
      LOGDEBUG("HIT\n");
      LOGDEBUG(*curr->op1, "\n");
      curr->op1 = reduceTree(curr->lop1);
      if (curr->op1)
        LOGDEBUG(*curr->op1, "\n");
      else {
        LOGERR("Bad op1 after reduce tree!\n");
        return nullptr;
      }
      if (instCache[curr->op1]->isLeaf()) {
        LOGDEBUG("CHANGE OP TYPE TO LEAF\n");
        curr->opType = LEAF; // mark self as leaf
      }
    } else {
      reduceTree(curr->lop1);
      reduceTree(curr->lop2);
    }
    return curr->val;
  }

  set<unsigned> _visited;
  void searchNext(Instruction *I) {
    // search next through back edges
    if (I == nullptr)
      return;
    int idx = getInstrToIndex(dyn_cast<Instruction>(I));
    if (idx == -1) {
      LOGERR("operand instruction not found\n");
      return;
    }
    if (_visited.count(idx) != 0)
      return;
    _visited.insert(idx);
    for (auto nextId : backEdges[idx]) {
      if (nextId == InstrToIndex[tgtInstr]) {
        recordLogicOps(IndexToInstr[nextId]);
      } else if (tgtAffectedInstrs.count(nextId) == 0) {
        // skip not affected instrs
        recordLogicOps(IndexToInstr[nextId]);
        searchNext(IndexToInstr[nextId]);
        continue;
      } else { // in the target AFFECTED INstrs and is not the target :)
        recordLogicOps(IndexToInstr[nextId]);
      }
    }
  }

  void recordLogicOps(Instruction *I) {
    // suppose our taint analysis is correct
    if (I == nullptr) {
      return;
    }
    // if I is not related, keep looking,
    // we construct the logic relation, the values must be already tainted
    LOGDEBUG(*I, "\n");
    switch (getInstructionType(I)) {
    case ALLOCA_INST:
      LOGDEBUG("Search alloca instr\n");
      searchNext(I);
      break;
    case CALL_INST:
      LOGDEBUG("Search call instr\n");
      if (I == tgtInstr) {
        instCache[I] = new LogicOps(I, nullptr, LEAF);
        foundTgtInstrInBackTrack = true;
        LOGINFO("WE FIND THE BASE!\n");
        return;
      } else {
        // This call instruction is still affected,
        // like retval_is_error() in vsftpd
        CallInst *callInst = dyn_cast<CallInst>(I);
        if (callInst->getType()->isVoidTy()) {
          LOGDEBUG("func call with no return ", *callInst, "\n");

          ///
          // %19 = bitcast %struct.fsal_status__* %3 to i8*, !dbg !296
          // %20 = bitcast %struct.fsal_status__* %7 to i8*, !dbg !296
          // call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %19, i8* align 4 %20, i64 8, i1 false), !dbg !296
          /// MemTransferInst 
          if (isa<MemCpyInst>(callInst)) {
            LOGDEBUG("this is memory copy", *callInst, "\n");
            MemCpyInst* MCI = dyn_cast<MemCpyInst>(callInst);
            Value *src = MCI->getSource();
            Value *tgt = MCI->getDest();
            if (isa<Instruction>(tgt) && isa<Instruction>(src)) {
              Instruction *tgtInst = dyn_cast<Instruction>(tgt);
              Instruction *srcInst = dyn_cast<Instruction>(src);
              instCache[tgtInst] = new LogicOps(tgtInst, srcInst, EQUIV);
              LOGDEBUG("source, ", *src, "\n");
              LOGDEBUG("target, ", *tgt, "\n");
            }
          }
          break;
        }
        // This is only for arguments
        if (callInst->getNumArgOperands() > 1) {
          LOGERR("Cannot handle args larger than 1 for now!");
        }
        for (unsigned i = 0; i < callInst->getNumArgOperands(); ++i) {
          // errs() << i <<"\t arg operand\n"<< *callInst->getArgOperand(i)
          // <<"\n";
          Instruction *tmp = dyn_cast<Instruction>(callInst->getArgOperand(i));
          if (tmp == nullptr)
            continue;
          instCache[I] = new LogicOps(I, tmp, LogicType::CALL);
        }
        searchNext(I);
      }
      break;
    case STORE_INST: { // this is special, do not care about store itself!
      LOGDEBUG("Search store instr\n");
      StoreInst *inst = dyn_cast<StoreInst>(I);
      // we don't care about the currIdx itself

      Value *tgt = inst->getOperand(1); // tgt depends on src
      Value *src = inst->getOperand(0);
      if (isa<Instruction>(tgt) && isa<Instruction>(src)) {
        if (isa<BitCastInst>(tgt)) {
          tgt = dyn_cast<BitCastInst>(tgt)->getOperand(0);
          LOGDEBUG("handle store to a bitcast inst, ", *tgt ,"\n");
        }
        Instruction *tgtInst = dyn_cast<Instruction>(tgt);
        Instruction *srcInst = dyn_cast<Instruction>(src);
        instCache[tgtInst] = new LogicOps(tgtInst, srcInst, EQUIV);
      }
      searchNext(I);
      break;
    }
    case LOAD_INST: {
      LOGDEBUG("Search load instr\n");
      LoadInst *inst = dyn_cast<LoadInst>(I);
      Value *src = inst->getOperand(0); // the pointer operand
      // TODO may store in global variable
      if (isa<Instruction>(src))
        instCache[I] = new LogicOps(I, dyn_cast<Instruction>(src), EQUIV);
      searchNext(I);
      break;
    }
    case ICMP_INST: {
      LOGDEBUG("Search icmp instr\n");
      ICmpInst *inst = dyn_cast<ICmpInst>(I);

      CmpInst::Predicate pred = inst->getPredicate(); // icmp ne

      Value *cmpsrc1 = inst->getOperand(0);
      Value *cmpsrc2 = inst->getOperand(1);

      if (isa<Instruction>(cmpsrc2) && isa<Constant>(cmpsrc1)) {
        auto tmp = cmpsrc2;
        cmpsrc2 = cmpsrc1;
        cmpsrc1 = tmp;
      }

      if (isa<Instruction>(cmpsrc1) && isa<Constant>(cmpsrc2)) {
        if (isa<ConstantInt>(cmpsrc2)) {
          Instruction *cmpsrc1Inst = dyn_cast<Instruction>(cmpsrc1);
          instCache[I] = new LogicOps(I, cmpsrc1Inst, ICMP);
          instCache[I]->pred = pred;
          instCache[I]->cmpInt = dyn_cast<ConstantInt>(cmpsrc2)->getZExtValue();
        } else if (isa<ConstantPointerNull>(cmpsrc2)) {
          Instruction *cmpsrc1Inst = dyn_cast<Instruction>(cmpsrc1);
          instCache[I] = new LogicOps(I, cmpsrc1Inst, ICMP);
          instCache[I]->pred = pred;
          // Use a special int to represent nullptr
          instCache[I]->cmpInt = NULLPTR_VAL;
        } else {
          LOGERR("Could not hanle ICMP with non-int or nullptr constant\n");
        }
      } else {

        LOGERR("Could not handle ICMP with two vars\n");
      }
      // keep looking through back edges
      searchNext(I);
      break;
    }
    case BR_INST: {
      LOGDEBUG("Search BR instr\n");
      BranchInst *inst = dyn_cast<BranchInst>(I);
      if (inst->isConditional()) { // jump instr has 1 operand
        if (isa<Instruction>(inst->getCondition()))
          instCache[I] =
              new LogicOps(I, dyn_cast<Instruction>(inst->getCondition()), BR);
      }
      // if (! isa<Instruction>(inst->getCondition())) {
      //     break;
      // }
      searchNext(I);
      break;
    }
    case BINARY_OP_INST:
      LOGDEBUG("Search binop instr\n");
      searchNext(I);
      break;
    case GETELEMENTPTR_INST: {
      LOGDEBUG("Search getelementptr instr\n");
      // status.major == SUCCESS ;

      GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(I);

      if (gep->getNumOperands() == 3) { // handle the simple case
        int field = -1;
        Value *v2 = gep->getOperand(2);
        if (isa<ConstantInt>(v2)) {
          field = dyn_cast<ConstantInt>(v2)->getSExtValue();
          Type *t = gep->getOperand(0)->getType();
          Type *elemt = isa<PointerType>(t)
                            ? dyn_cast<PointerType>(t)->getElementType()
                            : t;

          if (isa<StructType>(elemt)) {
            /*A field-sensitive approach*/
            Value *src1 = gep->getOperand(0);
            // I = src1.field
            instCache[I] = new LogicOps(I, dyn_cast<Instruction>(src1), GEPTR);
            instCache[I]->gepField = field;
          }
        }
      }
      searchNext(I);
      break;
    }
    case CAST_INST: {
      LOGDEBUG("Search cast instr\n");
      CastInst *inst = dyn_cast<CastInst>(I);
      instCache[I] =
          new LogicOps(I, dyn_cast<Instruction>(inst->getOperand(0)), EQUIV);
      searchNext(I);
      break;
    }
    case RETURN_INST: {
      LOGDEBUG("Search return instr\n");
      searchNext(I);
      break;
    }
    case TODO_INST:
      LOGDEBUG("Search OTHER instr\n");
      searchNext(I);
      break;
    }
  }

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
      std::cout << "Free memory " << cnt << " LogicInfo\n";
      if (it->second != nullptr) {
        std::cout << "Free memory " << cnt << " LogicInfo\n";
        delete it->second;
      }
    }
    EdgeToInfo.clear();
    std::cout << "Free memory " << cnt << " LogicInfo end...\n";
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

    std::cout << "Free memory " << cnt << " LogicInfo end...\n";
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

  bool isRetImpactedByInstr(Instruction *inst) {
    if (inst == nullptr)
      return false;
    LogicInfo *allInfo = lastInfo;
    int idx = getInstrToIndex(inst);
    // errs() << idx <<"\n";
    if (idx == -1) {
      LOGERR("The inst is not in the taint analyzed function!\n");
      assert(false);
    }
    set<unsigned> affectedIndices;
    allInfo->getImpactedInstIndicies(idx, &affectedIndices);
    // Find all ret instrs in the function;

    // Function* func = inst->getFunction();
    // for (auto insIter = inst_begin(func); insIter!= inst_end(func);++insIter)
    // {
    //     if (isa<ReturnInst>(insIter)) {
    //         LOGDEBUG("Found retInst", *instIter);
    //     }
    // }
    // allInfo->print(IndexToInstr, InstrToIndex);
    set<Instruction *> affectedInstrs;
    for (auto const &it : affectedIndices) {
      Instruction *inst = getIndexToInstr(it);
      if (isa<ReturnInst>(inst)) {
        affectedInstrs.insert(inst);
      }
    }

    LOGDEBUG("Found\t", affectedInstrs.size(), " return instructions\n");
    return affectedInstrs.size() != 0;
  }
  // should be called after run worklist
  void getImpactedInstr(Instruction *inst, set<Instruction *> *affectedInstrs,
                        ImpactedInstrType instType) {
    if (inst == nullptr)
      return;

    // LogicInfo* allInfo = getLastInfo();;
    LogicInfo *allInfo = lastInfo;

#ifdef LOG_LEVEL_DEBUG
    allInfo->print();
#endif

    int idx = getInstrToIndex(inst);
    LOGDEBUG("trying to find the impacted instrs of ", *inst, "with idx ", idx, "\n");
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

  // should be called after run worklist
  void getImpactedInstrIndex(Instruction *inst, set<unsigned> *affectedInstrs,
                             ImpactedInstrType instType) {
    if (inst == nullptr)
      return;

    LogicInfo *allInfo = lastInfo;

    int idx = getInstrToIndex(inst);
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
      // Instruction *inst = getIndexToInstr(it);
      if (instType == ALL_TYPE) {
        cnt += 1;
        affectedInstrs->insert(it);
      }
    }
    LOGINFO("affected ", cnt, " instrs\n");
    printSet(affectedInstrs, "\n");
  }

  LogicInfo *getLastInfo() {
    const auto it = EdgeToInfo.rbegin();
    errs() << "Edge " << it->first.first
           << "->"
              "Edge "
           << it->first.second << ":\n";
    (it->second)->print(IndexToInstr, InstrToIndex);
    return it->second;
  }

  void flowfunction(Instruction *I, std::vector<unsigned> &IncomingEdges,
                    std::vector<unsigned> &OutgoingEdges, LogicInfo *outInfo) {
    auto currIdx = getInstrToIndex(I);
    if (currIdx == -1) {
      LOGERR("Flow function met unexpected instruction\n");
      return;
    }
    for (auto &i : IncomingEdges) {
      Edge e = std::make_pair(i, currIdx);
      LogicInfo::join(outInfo, getEdgeToInfo(e), outInfo);
    }
    // LOGERR("here! enter switch!!\n");
    switch (getInstructionType(I)) {
    case CALL_INST: { // the current idx depend on function parameters
      LOGDEBUG("handle call instr", *I, "\n");
      CallInst *callInst = dyn_cast<CallInst>(I);
      // to learn about the call function, the callInst->getNumOperands()-1 is
      // the function

      if (callInst->getType()->isVoidTy()) {
        LOGDEBUG("func call with no return ", *callInst, "\n");
        ///
        // %19 = bitcast %struct.fsal_status__* %3 to i8*, !dbg !296
        // %20 = bitcast %struct.fsal_status__* %7 to i8*, !dbg !296
        // call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %19, i8* align 4 %20, i64 8, i1 false), !dbg !296
        /// MemTransferInst 
        if (isa<MemCpyInst>(callInst)) {
          LOGDEBUG("this is memory copy", *callInst, "\n");
          MemCpyInst* MCI = dyn_cast<MemCpyInst>(callInst);
          Value *src = MCI->getSource();
          Value *tgt = MCI->getDest();
          if (isa<Instruction>(tgt) && isa<Instruction>(src)) {
            int tgt_idx = getInstrToIndex(dyn_cast<Instruction>(tgt));
            int src_idx = getInstrToIndex(dyn_cast<Instruction>(src));
            if (tgt_idx == -1 || src_idx == -1) {
              LOGERR("operand instruction not found", *src, "\n");
              break;
            }
            LOGDEBUG("source, ", *src, "\n");
            LOGDEBUG("target, ", *tgt, "\n");
            LOGDEBUG(tgt_idx, "\t", src_idx, "\n");
            outInfo->insert(tgt_idx, src_idx);
          }
        }
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
      LOGDEBUG("handle store instr", *I, "\n");
      StoreInst *inst = dyn_cast<StoreInst>(I);
      // we don't care about the currIdx itself

      Value *tgt = inst->getOperand(1); // tgt depends on src
      Value *src = inst->getOperand(0);
      if (isa<Instruction>(tgt) && isa<Instruction>(src)) {
        // handle store to a bitcast instruction
        if (isa<BitCastInst>(tgt)) {
          tgt = dyn_cast<BitCastInst>(tgt)->getOperand(0);
          LOGDEBUG("handle store to a bitcast inst, ", *tgt ,"\n");
        }
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
        // LOGERR("before!\n");
        // outInfo->print();
        outInfo->replace(tgt_idx, src_idx);
        // LOGERR("after!", tgt_idx, "\t", src_idx, "\n");
        // outInfo->print();
        // NEED this for backtrack to work..
        outInfo->insert(currIdx, src_idx);
      }
      break;
    }
    case LOAD_INST: {
      LOGDEBUG("handle load instr", *I, "\n");
      LoadInst *inst = dyn_cast<LoadInst>(I);
      Value *src = inst->getOperand(0); // the pointer operand
      LOGDEBUG("src ", *src, "\n");
      if (!isa<Instruction>(src))
        break;
      int idx = getInstrToIndex(dyn_cast<Instruction>(src));
      // LOGDEBUG("srcidx ", idx, "\n");
      // LOGDEBUG("curridx", currIdx, "\n");
      /* vsftpd
       * Bug: load a global variable that is not recorded.
       * %16 = load i8*, i8** @do_file_send_rwloop.p_readbuf, align 8
       * load a global variable
       */
      if (idx == -1) {
        LOGERR("operand instruction not found", *src, "\n");
        break;
      }
      // LOGERR("load before\n");
      // outInfo->print();
      outInfo->insert(currIdx, idx);
      // LOGERR("load after\n");
      // outInfo->print();
      break;
    }
    case ICMP_INST: {
      LOGDEBUG("handle icmp instr", *I, "\n");
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
      LOGDEBUG("handle branch instr", *I, "\n");
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
    case RETURN_INST: {
      LOGDEBUG("handle return instr", *I, "\n");
      ReturnInst *inst = dyn_cast<ReturnInst>(I);
      Value *retVal = inst->getReturnValue();
      if (retVal == nullptr) // fix bug
        break;
      if (!isa<Instruction>(retVal))
        break;
      Instruction *tmp = (Instruction *)inst->getReturnValue();
      int idx = getInstrToIndex(tmp);
      if (idx == -1) {
        LOGERR("operand", *tmp, " is not instr\n");
        break;
      }
      LOGDEBUG(currIdx, idx, "\n");
      outInfo->insert(currIdx, idx);

      break;
    }
    case GETELEMENTPTR_INST: {
      // we don't really care about the concrete value, just need to know which
      // value it depends on, such as if the address/value is dependent on x

      // just make the cache
      GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(I);

      if (gep->getNumOperands() == 3) { // handle the simple case
        int field = -1;
        Value *v2 = gep->getOperand(2);
        if (isa<ConstantInt>(v2)) {
          field = dyn_cast<ConstantInt>(v2)->getSExtValue();

          Type *t = gep->getOperand(0)->getType();

          Type *elemt = isa<PointerType>(t)
                            ? dyn_cast<PointerType>(t)->getElementType()
                            : t;

          if (isa<StructType>(elemt)) {
            // StructType * selemt = dyn_cast<StructType>(elemt);
            // StringRef tName = stripStructName(selemt->getName());
            // now we know it is great match!
            /*A field-sensitive approach*/
            Value *src1 = gep->getOperand(0);
            if (isa<LoadInst>(src1)) {
              LoadInst *lins = dyn_cast<LoadInst>(src1);
              Value *lsrc = lins->getOperand(0);
              LOGDEBUG("real src\t", *lsrc, "\n", "field\t", field, "\n",
                       "gepinst", *gep, "\n");

              if (Instruction *realSrc = dyn_cast<Instruction>(lsrc)) {
                fieldDepPairs[I] = make_pair(realSrc, field);
                // only break here, treat it as a new variable
                break;
              }
            }
          }
        }
      } 
    } 
    // fall through
    case BINARY_OP_INST:
    case CAST_INST:
      // bitcast/truc/sext/zext, unary op
      LOGDEBUG("handle cast instr", *I, "\n");
      if (isa<BitCastInst> (I)) {
        // BitCastInst *BCI = dyn_cast<BitCastInst>(I); 
        // place holder -> need to store it as tmp values
        // so that when doing memcopy/store, directly use
        // source instruction
        // if (isa<Instruction>(I->getOperand(0))) {
        //   int idx = getInstrToIndex((Instruction *)I->getOperand(0));
        //   outInfo->insert(currIdx, idx);
        // }
      }
      // fall through
    default:
      LOGDEBUG("handled with debug ", *I, "\n");
      if (I->getType()->isVoidTy()) {
        LOGDEBUG("skip if no return\n");
        break;
      }
      for (unsigned i = 0; i < ((User *)I)->getNumOperands(); ++i) {
        if (!isa<Instruction>(I->getOperand(i)))
          continue;
        int idx = getInstrToIndex((Instruction *)I->getOperand(i));
        if (idx == -1) {
          LOGERR("operand", *I->getOperand(i), " is not instr\n");
          continue;
        }
        outInfo->insert(currIdx, idx);
      }
      break;
    }

    // outInfo->print(IndexToInstr, InstrToIndex);
    // for (size_t i =0;i<OutgoingEdges.size(); ++i) {
    //     Infos.push_back(outInfo);
    // }
    // LOGDEBUG("handled with debug done", "\n");

    return;
  }

public:
  LogicAnalysis(LogicInfo &bottom, LogicInfo &rInfo)
      : DataFlowAnalysis(bottom, rInfo) {}
}; // end of subclass DFA

#endif // LogicRelation_ANALYZER