// for struct related classes

#ifndef _STRUCT_H
#define _STRUCT_H

#include "ResultCheckFunc.h"
#include "Utils.h"

struct StructFieldPair {
  string sname; // the struct's name e.g. authz_provider
  int field;    // which one is the check function, start from 0
  StructFieldPair(string s, int i) {
    sname = s;
    field = i;
  }
};

/**
 * struct name is more difficult to strip than function names.
 * First, they have a prefix "struct." (it's simple), but it has suffix like
 *
 * struct._disk_unique_identifier_t.8834
 * struct.anon.149.14720
 *
 * and also, it could be:
 *
 * union.anon.464945
 *
 * So, here is the algorithm. we go from the back to the front, and
 * filter all the .12345 like tails.
 */
string stripStructName(string stname) {
  string stripedname = stname;
  if (stripedname.substr(0, 7).compare("struct.") == 0) {
    stripedname = stripedname.substr(7);
  }

  while (true) {
    if (stripedname.rfind('.') != string::npos) {
      unsigned dp = stripedname.rfind('.');
      string sub = stripedname.substr(dp + 1);
      if (isNumeric(sub)) {
        stripedname = stripedname.substr(0, dp);
      } else {
        break;
      }
    } else {
      break;
    }
  }

  return stripedname;
}

Type *getPointedType(Type *t) {
  Type *ret = t;

  while (isa<PointerType>(ret)) {
    ret = dyn_cast<PointerType>(ret)->getElementType();
  }

  return ret;
}

string getStructName(Type *tx) {
  Type *ty = getPointedType(tx);

  string structName = "";

  if (ty->getTypeID() == Type::StructTyID) {
    StructType *STy = cast<StructType>(ty);

    if (!STy->isLiteral()) {
      // Literal structs do not have name
      structName = STy->getName().str();
    }
  }
  // errs() << structName << "\n";
  return stripStructName(structName);
}

string getStructName(GetElementPtrInst *gepi) {
  PointerType *pType = dyn_cast<PointerType>(gepi->getPointerOperandType());
  Type *t = pType->getElementType();
  return getStructName(t);
}

StructFieldPair *gepMatchList(GetElementPtrInst *gepInst,
                              list<StructFieldPair *> sfps) {
  if (gepInst->getNumOperands() < 3) {
    return nullptr;
  }
  string sname = getStructName(gepInst);
  uint64_t field = UINT64_MAX;

  Value *v = gepInst->getOperand(2);
  if (isa<ConstantInt>(v)) {
    field = dyn_cast<ConstantInt>(v)->getZExtValue();
  }
  LOGINFO("Trying to match sfp: ", sname, "\t", field, "\n");
  if (sname == "" || field == UINT64_MAX) {
    return nullptr;
  }

  // match sfp in list
  for (auto &sfp : sfps) {
    if (sfp->sname.compare(sname) == 0 && (unsigned)sfp->field == field) {
      LOGINFO("Matched ", sname, "\t", field, "\n");
      return sfp;
    }
    continue;
  }

  return nullptr;
}

StructFieldPair *
gepMatchFromMap(GetElementPtrInst *gepInst,
                map<StructFieldPair *, ResultCheckFunc *> mps) {
  if (gepInst->getNumOperands() < 3) {
    return nullptr;
  }
  string sname = getStructName(gepInst);
  uint64_t field = UINT64_MAX;

  Value *v = gepInst->getOperand(2);
  if (isa<ConstantInt>(v)) {
    field = dyn_cast<ConstantInt>(v)->getZExtValue();
  }
  // LOGINFO("Trying to match sfp: ", sname, "\t", field, "\n");
  if (sname == "" || field == UINT64_MAX) {
    return nullptr;
  }

  // match sfp in list
  for (auto &it : mps) {
    if (it.first->sname.compare(sname) == 0 &&
        (unsigned)it.first->field == field) {
      LOGINFO("Matched ", sname, "\t", field, "\n");
      return it.first;
    }
    continue;
  }

  return nullptr;
}

void processGEPInstr(
    Instruction *I,
    map<Instruction *, set<Instruction *> *>
        &mpStructToGeps, // struct to all uses of the struct
    map<Instruction *, pair<Instruction *, int>> &fieldDepPairs) {
  GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(I);
  if (gep == nullptr)
    return;

  if (gep->getNumOperands() == 3) { // handle the simple case
    int field = -1;
    Value *v2 = gep->getOperand(2);
    if (isa<ConstantInt>(v2)) {
      field = dyn_cast<ConstantInt>(v2)->getSExtValue();
      Type *t = gep->getOperand(0)->getType();
      Type *elemt =
          isa<PointerType>(t) ? dyn_cast<PointerType>(t)->getElementType() : t;

      if (isa<StructType>(elemt)) {
        // StructType * selemt = dyn_cast<StructType>(elemt);
        // StringRef tName = stripStructName(selemt->getName());
        // now we know it is great match!
        /*A field-sensitive approach*/
        Value *src1 = gep->getOperand(0);
        if (isa<LoadInst>(src1)) {
          LoadInst *lins = dyn_cast<LoadInst>(src1);
          Value *lsrc = lins->getOperand(0);
          LOGDEBUG("real src\t", *lsrc, "\n", "field\t", field, "\n", "gepinst",
                   *gep, "\n");

          if (Instruction *realSrc = dyn_cast<Instruction>(lsrc)) {
            fieldDepPairs[I] = make_pair(realSrc, field);

            if (mpStructToGeps.find(realSrc) == mpStructToGeps.end()) {
              mpStructToGeps[realSrc] = new set<Instruction *>();
              mpStructToGeps[realSrc]->insert(I);
            } else {
              mpStructToGeps[realSrc]->insert(I);
            }
            return;
          }
        }
      }
    }
  }
}

#endif