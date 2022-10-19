// the config for one target program
//
#ifndef _CONFIG_H_
#define _CONFIG_H_

#include "Utils.h"

class Config {
public:
  string _softwareName;
  string _bcfile;

  // file name that contains functions
  string _check_file;
  string _struct_file;
  string _libcall_file;
  string _exit_file;

  Config() {}

  void initConfig(string configFile) {
    ifstream infile(configFile);
    string line;
    string directive, value;

    if (!infile.is_open()) {
      // can not open file; use exception to abort early :)
      LOGERR(configFile + "not found");
      return;
    }

    while (getline(infile, line)) {
      if (line.find_first_of('#', 0) == 0) {
        continue;
      }
      istringstream iss(line);
      iss >> directive >> value;

      if (directive.compare("target") == 0) {
        _softwareName = value;
      }
      if (directive.compare("bcfile") == 0) {
        _bcfile = value;
      } else if (directive.compare("AccessCheckFunctions") == 0) {
        _check_file = value;
      } else if (directive.compare("StructCheckFunctions") == 0) {
        _struct_file = value;
      } else if (directive.compare("LibraryCallFunctions") == 0) {
        _libcall_file = value;
      } else if (directive.compare("ExitCallFunctions") == 0) {
        _exit_file = value;
      }
    }

    // target and bcfiles should always be set
    if (_softwareName == "" || _bcfile == "") {
      LOGERR("Required field in the .conf is not set.");
      // throw std::runtime_error("Required file not found");
      return;
    }
  }
};

#endif // end config class