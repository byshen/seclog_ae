#include "Utils.h"

void LOG() {}
void MYLOG() {}
void LOGINFO() {}
void LOGERR() {}
void LOGDEBUG() {}

bool listStringEqual(list<string> a, list<string> b) {

  if (a.size() != b.size())
    return false;

  list<string>::iterator ii = a.begin(), ie = a.end();
  list<string>::iterator ji = b.begin(), je = b.end();

  for (; ii != ie && ji != je; ii++, ji++) {
    if ((*ii).compare(*ji) != 0) {
      return false;
    }
  }
  return true;
}

bool isNumeric(const std::string &s) {
  std::string::const_iterator it = s.begin();
  while (it != s.end() && std::isdigit(*it))
    ++it;
  return !s.empty() && it == s.end();
}

string getDemangledName(string orgName) {
  string args = "/usr/bin/c++filt " + orgName + " >" + DEMANGLED_NAME;
  int systemRet = system(args.c_str());
  if (systemRet == -1) {
    LOGERR(args, "system() failed\n");
  }

  ifstream infile(DEMANGLED_NAME);
  string line;
  string demangledName;

  if (getline(infile, line)) {
    istringstream iss(line);
    iss >> demangledName;
    auto tmp = demangledName;
    if (demangledName.find('(') != string::npos) // remove (int)
      demangledName = demangledName.erase(demangledName.find_first_of("("));

    // LOGDEBUG("Demangled ", tmp, "\t", orgName, "\t", demangledName, "\n");
    return demangledName;
  } else {
    LOGERR("Demangled name is wrong!!\n");
    assert(false);
  }
  return NULL;
}

string int2string(int64_t number) {
  stringstream ss; // create a stringstream
  ss << number;    // add number to the stream
  return ss.str(); // return a string with the contents of the stream
}

string idx2string(int64_t idx) {
  if (idx == UINT_MAX) {
    return "*";
  } else {
    return int2string(idx);
  }
}

void prettyTimePrint(time_t second) {
  time_t hour = second / 3600;
  time_t res = second % 3600;
  time_t minute = res / 60;
  res = res % 60;

  LOG(hour, " hour, ", minute, " min, ", res, " sec.\n");
}

bool keywordMatch(string structname, list<string> *keywords) {
  //	string alphabet[]  = {"msg", "wafl_inode", "runtime", "buf", "anon",
  //"wafl_priv", "mystr", "session"};

  std::transform(structname.begin(), structname.end(), structname.begin(),
                 ::tolower);

  list<string>::iterator iter = keywords->begin(), end = keywords->end();
  for (; iter != end; iter++) {
    if (structname.find(*iter) != string::npos) {
      return true;
    }
  }

  return false;
}
