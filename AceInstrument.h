// main util functions in AceInstrument.cpp
#ifndef _AceInstrument_H_
#define _AceInstrument_H_

#include "Common.h"
#include "Config.h"
#include "MyCallGraph.h"
#include "ResultCheckFunc.h"
#include "Struct.h"
#include "Utils.h"
#include <unistd.h>

// constants, may use a global config later
#define CHECK_FUNC_FILE "check.func"
#define EXIT_FUNC_FILE "ex.func"
#define LIB_CALL_FUNC_FILE "libcall.func"
#define CONFIG_FILE "target.conf" // this is all configs

int initStructFuncs(string fileName,
                    list<StructFieldPair *> &checkStructFuncNames) {
  ifstream infile(fileName.c_str(), ios::in);
  if (!infile.is_open()) // can not open file; use exception to abort early :)
    throw std::ios_base::failure(fileName + "not found");

  string line;
  string structName;
  int field;
  int cnt = 0;
  while (getline(infile, line)) {
    cnt++;
    istringstream iss(line);
    iss >> structName >> field;
    LOGDEBUG(structName, "\n");
    structName = stripStructName(structName);
    checkStructFuncNames.push_back(new StructFieldPair(structName, field));
  }
  LOGINFO("read ", cnt, " struct from ", fileName, "\n");
  return 0;
}

int initStructFuncsWithRetval(
    string fileName,
    map<StructFieldPair *, ResultCheckFunc *> &checkFuncNames) {
  ifstream infile(fileName.c_str(), ios::in);
  if (!infile.is_open()) // can not open file; use exception to abort early :)
    throw std::ios_base::failure(fileName + "not found");

  string line;
  string funcName;
  string rcfType;
  string valCheckFuncName;
  int field;
  int val;
  int cnt = 0;
  // Parse the func file per line
  while (getline(infile, line)) {
    cnt++;
    // seprate each value by space
    istringstream iss(line);
    // check func name
    iss >> funcName;
    // check func name
    iss >> field;
    // return value
    iss >> val;
    // FIXME: what is rcfType
    iss >> rcfType;
    // FIXME: what is valCheckFuncName
    iss >> valCheckFuncName;

    if (funcName.find_first_of("#") == 0) // stop when read #
      break;
    LOGDEBUG(funcName, "\t", val, "\n");
    // checkFuncNames.push_back(funcName);
    // Store val

    StructFieldPair *sfp = new StructFieldPair(funcName, field);
    checkFuncNames[sfp] = new ResultCheckFunc(val);
    // Store rcfType
    if (rcfType == "EQ") {
      checkFuncNames[sfp]->type = ResultCheckFuncType::RESULT_EQ;
    } else if (rcfType == "NE") {
      checkFuncNames[sfp]->type = ResultCheckFuncType::RESULT_NE;
    } else if (rcfType == "LT") {
      checkFuncNames[sfp]->type = ResultCheckFuncType::RESULT_LT;
    } else if (rcfType == "GT") {
      checkFuncNames[sfp]->type = ResultCheckFuncType::RESULT_GT;
    } else if (rcfType == "LE") {
      checkFuncNames[sfp]->type = ResultCheckFuncType::RESULT_LE;
    } else if (rcfType == "GE") {
      checkFuncNames[sfp]->type = ResultCheckFuncType::RESULT_GE;
    } else {
      checkFuncNames[sfp]->type = ResultCheckFuncType::RESULT_NE;
    }

    // thr function to check retval of check func
    if (valCheckFuncName != "") {
      if (valCheckFuncName == "field") { // ugly hack here
        // TODO handle result check function with field checks
      } else {
        checkFuncNames[sfp]->setValCheckFuncName(valCheckFuncName);
      }
    }
  }
  LOGINFO("read ", cnt, " functions from ", fileName, "\n");

  return 0;
}

int initFuncs(string fileName, list<string> &checkFuncNames) {
  // errs() << fileName << "\n";
  ifstream infile(fileName.c_str(), ios::in);

  if (!infile.is_open()) // can not open file; use exception to abort early :)
    throw std::ios_base::failure(fileName + "not found");

  string line;
  string funcName;
  int cnt = 0;
  while (getline(infile, line)) {
    cnt++;
    istringstream iss(line);
    iss >> funcName;
    LOGDEBUG(funcName, "\n");
    checkFuncNames.push_back(funcName);
  }
  LOGINFO("read ", cnt, " functions from ", fileName, "\n");

  return 0;
}

int initFuncsWithRetval(string fileName,
                        map<string, ResultCheckFunc *> &checkFuncNames) {
  // errs() << fileName << "\n";
  ifstream infile(fileName.c_str(), ios::in);

  if (!infile.is_open()) // can not open file; use exception to abort early :)
    throw std::ios_base::failure(fileName + "not found");

  string line;
  string funcName;
  string rcfType;
  string valCheckFuncName;
  int val;
  string fieldVal;

  int cnt = 0;
  // Parse the func file per line
  while (getline(infile, line)) {
    // seprate each value by space
    istringstream iss(line);
    // check func name
    iss >> funcName;
    // return value
    iss >> val;
    // FIXME: what is rcfType
    iss >> rcfType;
    // FIXME: what is valCheckFuncName
    iss >> valCheckFuncName;

    if (funcName.find_first_of("#") == 0) // stop when read #
      break;

    cnt++;
    LOGDEBUG(funcName, "\t", val, "\n");
    // checkFuncNames.push_back(funcName);
    // Store val
    checkFuncNames[funcName] = new ResultCheckFunc(val);
    // Store rcfType
    if (rcfType == "EQ") {
      checkFuncNames[funcName]->type = ResultCheckFuncType::RESULT_EQ;
    } else if (rcfType == "NE") {
      checkFuncNames[funcName]->type = ResultCheckFuncType::RESULT_NE;
    } else if (rcfType == "LT") {
      checkFuncNames[funcName]->type = ResultCheckFuncType::RESULT_LT;
    } else if (rcfType == "GT") {
      checkFuncNames[funcName]->type = ResultCheckFuncType::RESULT_GT;
    } else if (rcfType == "LE") {
      checkFuncNames[funcName]->type = ResultCheckFuncType::RESULT_LE;
    } else if (rcfType == "GE") {
      checkFuncNames[funcName]->type = ResultCheckFuncType::RESULT_GE;
    } else {
      checkFuncNames[funcName]->type = ResultCheckFuncType::RESULT_NE;
    }

    // the function to check retval of check func
    if (valCheckFuncName != "") {
      if (valCheckFuncName == "field") { // ugly hack here
        // TODO handle result check function with field checks
        iss >> fieldVal;
        checkFuncNames[funcName]->setField(fieldVal);
      } else { 
        LOGDEBUG("valCheckFuncName", valCheckFuncName, "\t", val, "\n");
        if (valCheckFuncName[0] == '*') {
          // valCheckFuncName is like "*result_check_func_1"
          checkFuncNames[funcName]->setRealRCF(valCheckFuncName.substr(1));
        } 
        else {
          // this is for cases like vsf_sysutil_retval_is_error
          checkFuncNames[funcName]->setValCheckFuncName(valCheckFuncName);
        }
      }
    }

    // handle special case
    string rcfname;
    iss >> rcfname;
    if (rcfname != "" and rcfname[0] == '*') {
      checkFuncNames[funcName]->setRealRCF(rcfname.substr(1));
    }
  }
  LOGINFO("read ", cnt, " functions from ", fileName, "\n");
  
  return 0;
}

#endif