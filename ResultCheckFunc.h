#ifndef RESULT_CHECK_FUNC
#define RESULT_CHECK_FUNC

#include "MyCallGraph.h"
#include "Utils.h"

extern MiniCallGraph *g_callGraph;

enum ResultCheckFuncType {
  RESULT_EQ, // ==
  RESULT_NE, // !=
  RESULT_LT, // <
  RESULT_GT, // >
  RESULT_LE, // <=
  RESULT_GE  // >=
};

struct ResultCheckFunc {
  // for now, just a retvalue
  int denyVal;
  ResultCheckFuncType type;
  Function *valCheckFunc;

  // the real result check function
  // the arguments of this realRCF should be 
  // the same as the result result of access check func
  Function *realRCF;

  int field; // handle special return value; check_open_permission, return
             // status.major

  ResultCheckFunc(int v) { 
    denyVal = v; 
    field   = -1;
  }
  static const int BOOLEAN_FALSE = -1000;
  static const int BOOLEAN_TRUE = -2000;

  bool isBoolean() {
    return denyVal == BOOLEAN_FALSE || denyVal == BOOLEAN_TRUE;
  }
  bool chooseFalse() { return isBoolean() && denyVal == BOOLEAN_FALSE; }
  bool chooseTrue() { return isBoolean() && denyVal == BOOLEAN_TRUE; }

  bool isEquiv() { return type == RESULT_EQ || type == RESULT_NE; }

  void setValCheckFuncName(string fname) {
    valCheckFunc = g_callGraph->getFuncPtrByDemangledName(fname);
    if (valCheckFunc == nullptr) {
      LOGERR("Fault in initializing ResultCheckFunc, ", fname, "\n");
      assert(false);
    }
  }

  void setRealRCF(string fname) {
    LOGDEBUG(fname, "\n");
    realRCF = g_callGraph->getFuncPtrByDemangledName(fname);
    if (realRCF == nullptr) {
      LOGERR("Fault in initializing ResultCheckFunc, ", fname, "\n");
      assert(false);
    }
  }

  void setField(string fieldstr) { field = std::stoi(fieldstr); }
  bool hasField() { return field != -1; }
  bool hasValCheckFunc() { return valCheckFunc != nullptr; }

  bool isDenied(int v) { return v == denyVal; }
  bool isAllowed(int v) { return v != denyVal; }
};

#endif // RESULT_CHECK_FUNC