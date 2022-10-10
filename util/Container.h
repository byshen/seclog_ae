

#ifndef CONTAINER_H
#define CONTAINER_H

#include "../Utils.h"

struct DeniedBranchContainer {
//////////////////////////////////////    
    Instruction* brInstr;
    // which basic block is the denied one 
    int deniedBranch; 
    // impacted by which original call instruction of the AC check func 
    Instruction* sourceInstr;  

    bool hasLog;
    bool isLogAtUpperLevel;
    Instruction* logInstr;

////////////////////////////////////
    DeniedBranchContainer(Instruction* br, int de, Instruction* src) 
    : brInstr(br), deniedBranch(de), sourceInstr(src)
    {
        hasLog = false;
        isLogAtUpperLevel = false;
    } 

    BasicBlock* getDeniedBasicBlock() {
        BranchInst* inst = dyn_cast<BranchInst>(brInstr);
        BasicBlock* startBB = inst->getSuccessor(deniedBranch);
        return startBB;
    }
    
    bool hasLogLocation() {
        return hasLog;
    }

    Instruction* getLogInstruction() {
        return logInstr;
    } 

    void setLogLocation(bool hl, Instruction** inst) {
        hasLog= hl;
        logInstr = *inst;
    }

    void setIsLogAtUpperLevel(bool b) {
        isLogAtUpperLevel = b;
    }

    void print() {
        errs() << "Denied container\n"
        << "denied branch\t" << deniedBranch <<"\n"
        << "has log" << to_string(hasLog?1:0)<<"\n"
        << "is log upper level" << to_string(isLogAtUpperLevel?1:0)<<"\n" ;
    
    }
};

#endif // CONTAINER_H