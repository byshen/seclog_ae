//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef DATA_FLOW_ANALYSIS_H
#define DATA_FLOW_ANALYSIS_H

#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/raw_ostream.h"
#include <deque>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

enum MyInstrType {
  ALLOCA_INST,
  CALL_INST,
  STORE_INST,
  LOAD_INST,
  ICMP_INST,
  BR_INST,
  BINARY_OP_INST,
  GETELEMENTPTR_INST,
  CAST_INST,
  RETURN_INST,
  TODO_INST
};

enum ImpactedInstrType { BRANCH_TYPE, CALL_TYPE, ALL_TYPE };

MyInstrType getInstructionType(Value *v) {
  Instruction *inst = dyn_cast<Instruction>(v);
  if (isa<AllocaInst>(v)) {
    return ALLOCA_INST;
  }

  if (isa<CallInst>(v)) {
    return CALL_INST;
  } else if (isa<StoreInst>(v)) {
    return STORE_INST;
  } else if (isa<LoadInst>(v)) {
    return LOAD_INST;
  } else if (isa<ICmpInst>(v)) { // ignore fcmp instr for now (boolean)
    return ICMP_INST;
  } else if (isa<BranchInst>(v)) {
    return BR_INST;
  } else if (inst->isBinaryOp()) {
    return BINARY_OP_INST;
  } else if (isa<GetElementPtrInst>(v)) {
    return GETELEMENTPTR_INST;
  } else if (inst->isCast()) {
    return CAST_INST;
  } else if (isa<ReturnInst>(v)) {
    return RETURN_INST;
  } else if (isa<ZExtInst>(v)) {
  }

  return TODO_INST;
}

namespace llvm {

/*
 * This is the base class to represent information in a dataflow analysis.
 * For a specific analysis, you need to create a sublcass of it.
 */
class Info {
public:
  Info() {}
  Info(const Info &other) {}
  virtual ~Info(){};

  /*
   * Print out the information
   *
   * Direction:
   *   In your subclass you should implement this function according to the
   * project specifications.
   */
  virtual void print() = 0;

  /*
   * Compare two pieces of information
   *
   * Direction:
   *   In your subclass you need to implement this function.
   */
  static bool equals(Info *info1, Info *info2);
  /*
   * Join two pieces of information.
   * The third parameter points to the result.
   *
   * Direction:
   *   In your subclass you need to implement this function.
   */
  static bool join(Info *info1, Info *info2, Info *result);

  void setInputInfo(Info *info);

  void explicitReleaseMemory();
};

/*
 * This is the base template class to represent the generic dataflow analysis
 * framework For a specific analysis, you need to create a sublcass of it.
 */
template <class Info, bool Direction> class DataFlowAnalysis {

public:
  typedef std::pair<unsigned, unsigned> Edge;
  // Edge to information map
  std::map<Edge, Info *> EdgeToInfo;
  // backEdges and forward edges are relative,
  // for backward passes, the forward edge are backwards :)
  std::map<unsigned, set<unsigned>> backEdges;
  std::map<unsigned, set<unsigned>> forwardEdges;
  // The bottom of the lattice
  Info Bottom;
  // The initial state of the analysis
  Info InitialState;
  // EntryInstr points to the first instruction to be processed in the analysis
  Instruction *EntryInstr;
  Info *lastInfo;

  // Index to instruction map
  std::map<unsigned, Instruction *> IndexToInstr;
  // Instruction to index map
  std::map<Instruction *, unsigned> InstrToIndex;

  DataFlowAnalysis(Info &bottom, Info &initialState)
      : Bottom(bottom), InitialState(initialState), EntryInstr(nullptr) {
    lastInfo = new Info();
  }

  virtual ~DataFlowAnalysis() {}

  unsigned maxIndex;

  /*
   * Assign an index to each instruction.
   * The results are stored in InstrToIndex and IndexToInstr.
   * A dummy node (nullptr) is added. It has index 0. This node has only one
   * outgoing edge to EntryInstr. The information of this edge is InitialState.
   * Any real instruction has an index > 0.
   *
   * Direction:
   *   Do *NOT* change this function.
   *   Both forward and backward analyses must use it to assign
   *   indices to the instructions of a function.
   */
  void assignIndiceToInstrs(Function *F) {

    // Dummy instruction null has index 0;
    // Any real instruction's index > 0.
    InstrToIndex[nullptr] = 0;
    IndexToInstr[0] = nullptr;

    unsigned counter = 1;
    for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
      Instruction *instr = &*I;
      InstrToIndex[instr] = counter;
      IndexToInstr[counter] = instr;
      maxIndex = counter;
      counter++;
    }

    return;
  }

  /*unsigned getInstrToIndex(Instruction * I) {
  return InstrToIndex[I];
}
   * Utility function:
   *   Get incoming edges of the instruction identified by index.
   *   IncomingEdges stores the indices of the source instructions of the
incoming edges.
   */
  void getIncomingEdges(unsigned index, std::vector<unsigned> *IncomingEdges) {
    assert(IncomingEdges->size() == 0 && "IncomingEdges should be empty.");

    for (auto const &it : EdgeToInfo) {
      if (it.first.second == index)
        IncomingEdges->push_back(it.first.first);
    }

    return;
  }

  /*
   * Utility function:
   *   Get incoming edges of the instruction identified by index.
   *   OutgoingEdges stores the indices of the destination instructions of the
   * outgoing edges.
   */
  void getOutgoingEdges(unsigned index, std::vector<unsigned> *OutgoingEdges) {
    assert(OutgoingEdges->size() == 0 && "OutgoingEdges should be empty.");

    for (auto const &it : EdgeToInfo) {
      if (it.first.first == index)
        OutgoingEdges->push_back(it.first.second);
    }

    return;
  }

  /*
   * Utility function:
   *   Insert an edge to EdgeToInfo.
   *   The default initial value for each edge is bottom.
   */
  void addEdge(Instruction *src, Instruction *dst, Info *content) {
    Edge edge = std::make_pair(InstrToIndex[src], InstrToIndex[dst]);
    forwardEdges[InstrToIndex[src]].insert(InstrToIndex[dst]);
    backEdges[InstrToIndex[dst]].insert(InstrToIndex[src]);
    if (EdgeToInfo.count(edge) == 0)
      EdgeToInfo[edge] = content;
    return;
  }

  /*
   * Initialize EdgeToInfo and EntryInstr for a forward analysis.
   */
  void initializeForwardMap(Function *func) {
    assignIndiceToInstrs(func);

    for (Function::iterator bi = func->begin(), e = func->end(); bi != e;
         ++bi) {
      BasicBlock *block = &*bi;

      Instruction *firstInstr = &(block->front());

      // Initialize incoming edges to the basic block
      // Predecessors - List of BasicBlocks that have this Interval's header
      // block as one of their successors.
      for (auto pi = pred_begin(block), pe = pred_end(block); pi != pe; ++pi) {
        BasicBlock *prev = *pi;
        Instruction *src = (Instruction *)prev->getTerminator();
        Instruction *dst = firstInstr;
        addEdge(src, dst, &Bottom); // initialize edge between  end of previous
                                    // blocks  and first instr with Bottom.
      }

      // If there is at least one phi node, add an edge from the first phi node
      // to the first non-phi node instruction in the basic block.
      if (isa<PHINode>(firstInstr)) {
        addEdge(firstInstr, block->getFirstNonPHI(), &Bottom);
      }

      // Initialize edges within the basic block
      for (auto ii = block->begin(), ie = block->end(); ii != ie; ++ii) {
        Instruction *instr = &*ii;
        if (isa<PHINode>(instr))
          continue;
        if (instr == (Instruction *)block->getTerminator())
          break;

        Instruction *next = instr->getNextNode();
        addEdge(instr, next, &Bottom);
      }

      // Initialize outgoing edges of the basic block
      // why need to initialize outgoing edges again? since incoming edges
      // already done??? => see addEdge implementation it is ok, because only
      // insert when the map does not contain that info.
      Instruction *term = (Instruction *)block->getTerminator();
      for (auto si = succ_begin(block), se = succ_end(block); si != se; ++si) {
        BasicBlock *succ = *si;
        Instruction *next = &(succ->front());
        addEdge(term, next, &Bottom);
      }
    }

    EntryInstr = (Instruction *)&((func->front()).front());
    addEdge(nullptr, EntryInstr, &InitialState);

    return;
  }

  /*
   * Direction:
   *   Implement the following function in part 3 for backward analyses
   */
  void initializeBackwardMap(Function *func) {
    assignIndiceToInstrs(func);

    for (Function::iterator bi = func->begin(), e = func->end(); bi != e;
         ++bi) {
      BasicBlock *block = &*bi;

      Instruction *firstInstr = &(block->front());

      // Initialize incoming edges to the basic block
      // Predecessors - List of BasicBlocks that have this Interval's header
      // block as one of their successors.
      for (auto pi = pred_begin(block), pe = pred_end(block); pi != pe; ++pi) {
        BasicBlock *prev = *pi;
        Instruction *src = (Instruction *)prev->getTerminator();
        Instruction *dst = firstInstr;
        addEdge(dst, src, &Bottom); // initialize edge between  end of previous
                                    // blocks  and first instr with Bottom.
      }

      // If there is at least one phi node, add an edge from the first phi node
      // to the first non-phi node instruction in the basic block.
      if (isa<PHINode>(firstInstr)) {
        addEdge(block->getFirstNonPHI(), firstInstr, &Bottom);
      }

      // Initialize edges within the basic block
      for (auto ii = block->begin(), ie = block->end(); ii != ie; ++ii) {

        Instruction *instr = &*ii;

        if (instr->getOpcode() == 1) {
          EntryInstr = instr;
          // errs() << "found ret\n";
        }
        if (isa<PHINode>(instr))
          continue;
        if (instr == (Instruction *)block->getTerminator())
          break;

        Instruction *next = instr->getNextNode();
        addEdge(next, instr, &Bottom);
      }

      // Initialize outgoing edges of the basic block
      // why need to initialize outgoing edges again? since incoming edges
      // already done??? => see addEdge implementation it is ok, because only
      // insert when the map does not contain that info.
      Instruction *term = (Instruction *)block->getTerminator();
      for (auto si = succ_begin(block), se = succ_end(block); si != se; ++si) {
        BasicBlock *succ = *si;
        Instruction *next = &(succ->front());
        addEdge(next, term, &Bottom);
      }
    }

    // entryInstr should be at the return point
    // EntryInstr = (Instruction *) &((func->back()).back());

    assert(EntryInstr != nullptr && "Entry instruction is null.");
    addEdge(nullptr, EntryInstr, &InitialState);

    return;
  }

  /*
   * The flow function.
   *   Instruction I: the IR instruction to be processed.
   *   std::vector<unsigned> & IncomingEdges: the vector of the indices of the
   * source instructions of the incoming edges. std::vector<unsigned> &
   * IncomingEdges: the vector of indices of the source instructions of the
   * outgoing edges. std::vector<Info *> & Infos: the vector of the newly
   * computed information for each outgoing eages.
   *
   * Direction:
   * 	 Implement this function in subclasses.
   */
  // virtual void flowfunction(Instruction * I,
  // 				std::vector<unsigned> & IncomingEdges,
  // 				std::vector<unsigned> & OutgoingEdges,
  // 				std::vector<Info *> & Infos) = 0;
  virtual void flowfunction(Instruction *I,
                            std::vector<unsigned> &IncomingEdges,
                            std::vector<unsigned> &OutgoingEdges,
                            Info *outInfo) = 0;

public:
  /*
   * Print out the analysis results.
   *
   * Direction:
   * 	 Do not change this funciton.
   * 	 The autograder will check the output of this function.
   */
  void print() {
    for (auto const &it : EdgeToInfo) {
      errs() << "Edge " << it.first.first
             << "->"
                "Edge "
             << it.first.second << ":\n";
      (it.second)->print(IndexToInstr, InstrToIndex);
    }
  }

  int getInstrToIndex(Instruction *I) {
    if (InstrToIndex.count(I) == 0) {
      return -1;
    }
    // bug: if I == nullptr, return 0  :(
    return InstrToIndex[I];
  }

  Instruction *getIndexToInstr(unsigned i) { return IndexToInstr[i]; }

  Info *getEdgeToInfo(Edge e) {
    if (EdgeToInfo.count(e) == 0) {
      errs() << "not found\n";
      return nullptr;
    }
    return EdgeToInfo[e];
  }

  /*
   * This function implements the work list algorithm in the following steps:
   * (1) Initialize info of each edge to bottom
   * (2) Initialize the worklist
   * (3) Compute until the worklist is empty
   *
   * Direction:
   *   Implement the rest of the function.
   *   You may not change anything before "// (2) Initialize the worklist".
   */

  void runWorklistAlgorithm(Function *func) {
    // (1) Initialize info of each edge to bottom

    std::cout << "worklist begin\n";
    // errs() << "worklist\n";
    if (Direction)
      initializeForwardMap(func);
    else
      initializeBackwardMap(func);

    assert(EntryInstr != nullptr && "Entry instruction is null.");

    // (2) Initialize the work list
    std::deque<unsigned> worklist;

    // not sure if order matters ...
    // yes, the order matters!
    // for (auto const &it : InstrToIndex) {
    // 	// errs()<< it <<"\n";
    // 	worklist.push_back(it.second);
    // }
    for (unsigned i = 0; i < maxIndex; ++i) {
      worklist.push_back(i);
    }

    std::cout << "init worklist done"
              << "\n";
    // (3) Compute until the work list is empty

    // for (auto i : EdgeToInfo) {
    // 	errs() << i.first.first << "\t" << i.first.second;
    // 	i.second->print();
    // 	errs() << "\n";
    // }
    int iterations = 0;
    while (worklist.size() > 0) {
      iterations++;
      // worklist.size()
      auto n = worklist.front();
      worklist.pop_front();
      if (n == 0)
        continue;
      // std::cout << n << "\n";
      // errs() << *getIndexToInstr(n) <<"\n";

      std::vector<unsigned> _inEdges;
      std::vector<unsigned> _outEdges;
      // std::vector<Info*> _infos; // saves outInfo of all edges;

      Info *outInfo = new Info();

      getIncomingEdges(n, &_inEdges);

      getOutgoingEdges(n, &_outEdges);

      flowfunction(IndexToInstr[n], _inEdges, _outEdges,
                   outInfo); // passed an empty set for outgoing edges
      // errs() << "infos" << _infos.size() << "\n";

      // stupid implementation
      // to fix the BUG : for return, no outedges, value is ignored in the
      // lastInfo:(
      lastInfo->setInputInfo(outInfo);

      for (unsigned i = 0; i < _outEdges.size(); ++i) {
        Edge _edge = std::make_pair(n, _outEdges[i]);
        Info new_info;

        // bug here, when merge the first two into new info
        // also change the original info
        // in fact, the outinfo should always be the new one.
        bool changedEdge = Info::join(EdgeToInfo[_edge], outInfo, &new_info);

        // if(! Info::equals(new_info, EdgeToInfo[_edge]) ) {
        // 	EdgeToInfo[_edge]->setInfo(new_info.taintIndicies);
        // 	worklist.push_back(_outEdges[i]);
        // }

        if (changedEdge) {
          worklist.push_back(_outEdges[i]);
        }
      }

      delete outInfo;
      // // free _infos; _infos are all the same, so just free one is enough;
      // auto it = _infos.begin();
      // delete (*it);
    }

    // set the lastInfo
    // lastInfo = EdgeToInfo.rbegin()->second;

    std::cout << "worklist ends with " << iterations << " iterations\n";
  }
};

} // namespace llvm
#endif // End DATA_FLOW_ANALYSIS_H