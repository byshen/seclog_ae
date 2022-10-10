#ifndef _UTILS_H_
#define _UTILS_H_

#include "llvm/Analysis/CFG.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <assert.h>
#include <climits>
#include <exception>
#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <time.h>

using namespace std;
using namespace llvm;

// for store slice results
// default is .second is -1;
// if it is a struct, the .second is not -1;
typedef std::pair<int, int> ParamPos;

// Global Debug Flag
// #define ACE_DEBUG
#define LOG_LEVEL_INFO
// #define LOG_LEVEL_DEBUG
#define LOG_LEVEL_ERR

// MANGLE
#define DEMANGLED_NAME "dem_name"

void LOG();
void MYLOG();
void LOGINFO();
void LOGERR();
void LOGDEBUG();

template <typename Head, typename... Args>
void MYLOG(const Head &head, const Args &... args) {
  llvm::errs() << head;
  MYLOG(args...);
}

template <typename Head, typename... Args>
void LOG(const Head &head, const Args &... args) {
  MYLOG(head, args...);
}

template <typename Head, typename... Args>
void LOGINFO(const Head &head, const Args &... args) {
#ifdef LOG_LEVEL_INFO
  llvm::errs() << "[info] ";
  MYLOG(head, args...);
#endif
}

template <typename Head, typename... Args>
void LOGDEBUG(const Head &head, const Args &... args) {
#ifdef LOG_LEVEL_DEBUG
  llvm::errs() << "[debug] ";
  MYLOG(head, args...);
#endif
}

template <typename Head, typename... Args>
void LOGERR(const Head &head, const Args &... args) {
#ifdef LOG_LEVEL_ERR
  llvm::errs() << "[error] ";
  MYLOG(head, args...);
#endif
}

/////////////////////////////////////////////////////////////////////////////
///////////////////////////////List Operation////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

template <typename T> bool searchList(list<T> *l, T target) {
  typename list<T>::iterator iter = l->begin(), end = l->end();
  for (; iter != end; iter++) {
    if (*iter == target) {
      return true;
    }
  }
  return false;
}

template <typename T> void mergeList(list<T> *a, list<T> *b) {
  typename list<T>::iterator i = b->begin(), e = b->end();
  for (; i != e; i++) {

    bool exist = false;

    typename list<T>::iterator ii = a->begin(), ee = a->end();
    for (; ii != ee; ii++) {
      if (*ii == *i) {
        exist = true;
        break;
      }
    }
    if (exist == false) {
      //			errs() << "push back\n";
      a->push_back(*i);
    }
  }
}

template <typename T> bool listEqual(list<T> a, list<T> b) {

  if (a.size() != b.size())
    return false;

  typename list<T>::iterator ii = a.begin(), ie = a.end();
  typename list<T>::iterator ji = b.begin(), je = b.end();

  for (; ii != ie && ji != je; ii++, ji++) {
    if (*ii != *ji) {
      return false;
    }
  }
  return true;
}

bool listStringEqual(list<string> a, list<string> b);

template <typename T>
typename list<T>::iterator indexOfList(list<T> *l, T target) {
  typename list<T>::iterator iter = l->begin(), end = l->end();
  for (; iter != end; iter++) {
    if (*iter == target) {
      return iter;
    }
  }
  return end;
}

///*
// * merge all the unique elements in the array and delete all the common
// elements
// */
// template<typename T>
// void uniqueMergeList(list<T>* a, list<T>* b) {
//	typename list<T>::iterator bIter = b->begin(), bEnd = b->end();
//	for(; bIter != bEnd; bIter++) {
//		typename list<T>::iterator idx = indexOfList(a, *bIter);
//
//		if(idx != a->end()) {
//			a->erase(idx);
//		} else {
//			a->push_back(*bIter);
//		}
//	}
//}

template <typename T> void printList(list<T> *a) {
  if (a == NULL)
    return;

  typename list<T>::iterator iter = a->begin(), end = a->end();
  for (; iter != end; iter++) {
    errs() << *iter << "\t";
  }
  errs() << "\n";
}

template <typename T> bool existInList(list<T> *a, T b) {
  if (a == NULL)
    return false;

  typename list<T>::iterator iter = a->begin(), end = a->end();
  for (; iter != end; iter++) {
    if ((*iter) == b) {
      return true;
    }
  }
  return false;
}

// copy from b to a without modifying b
// skip the elements from the head
template <typename T> void copyList(list<T> *a, list<T> *b, unsigned skip) {
  if (b == NULL)
    return;

  assert(skip <= b->size());

  typename list<T>::iterator iter = b->begin(), end = b->end();

  for (unsigned i = 0; i < skip; i++) {
    iter++;
  }

  for (; iter != end; iter++) {
    a->push_back(*iter);
  }
}

// void mergeLList(list<BasicBlock*>* a, list<BasicBlock*>* b) {
//	list<BasicBlock*>::iterator i = b->begin(), e = b->end();
//	for(; i != e; i++) {
//
//		bool exist = false;
//
//		list<BasicBlock*>::iterator ii = a->begin(), ee = a->end();
//		for(; ii != ee; ii++) {
//			if(*ii == *i) {
//				exist = true;
//				break;
//			}
//		}
//		if(exist == false) {
//			errs() << "push back\n";
//			a->push_back(*i);
//		}
//	}
//}

/////////////////////////////////////////////////////////////////////////////
///////////////////////////////Set Operation/////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
template <typename T> void printSet(set<T> *a, string delem) {
  if (!a)
    return;

  typename set<T>::iterator iter = a->begin(), end = a->end();
  for (; iter != end; iter++) {
    errs() << *iter << delem;
  }
  errs() << "\n";
}

template <typename T> void printSetPairs(set<pair<T, T>> *a, string delem) {
  if (!a)
    return;

  auto iter = a->begin(), end = a->end();
  for (; iter != end; iter++) {
    errs() << (*iter).first << "\t" << (*iter).second << delem;
  }
  errs() << "\n";
}

template <typename T> void printSetInstr(set<T> *a, string delem) {
  if (!a)
    return;

  typename set<T>::iterator iter = a->begin(), end = a->end();
  for (; iter != end; iter++) {
    errs() << *(*iter) << delem;
  }
}

template <typename T, typename V> void printMap(map<T, V> *a, string delem) {
  if (!a)
    return;

  typename map<T, V>::iterator iter = a->begin(), end = a->end();
  for (; iter != end; iter++) {
    errs() << *(iter->first) << delem << iter->second << "\n";
  }
}

template <typename T> void printSet(set<T> *a) { printSet(a, "\n"); }

// copy from b to a without modifying b
template <typename T> void copyList(list<T> *a, list<T> *b) {
  if (b == NULL)
    return;

  typename list<T>::iterator iter = b->begin(), end = b->end();
  for (; iter != end; iter++) {
    a->push_back(*iter);
  }
}

// copy from b to a without modifying b
template <typename T> void copySet(set<T> *a, set<T> *b) {
  if (b == NULL)
    return;

  typename set<T>::iterator iter = b->begin(), end = b->end();
  for (; iter != end; iter++) {
    a->insert(*iter);
  }
}

// copy all the elements from list b to set a
template <typename T> void copyList2Set(set<T> *a, list<T> *b) {
  if (b == NULL)
    return;

  typename list<T>::iterator iter = b->begin(), end = b->end();
  for (; iter != end; iter++) {
    a->insert(*iter);
  }
}

// delete all the elements in B from A
template <typename T> void excludeSet(set<T> *a, set<T> *b) {
  if (b == NULL)
    return;

  typename set<T>::iterator iter = b->begin(), end = b->end();
  for (; iter != end; iter++) {
    a->erase(*iter);
  }
}

// whether set A contains set B
template <typename T> bool setContains(set<T> *a, set<T> *b) {
  if (b == NULL)
    return true;

  typename set<T>::iterator iter = b->begin(), end = b->end();
  for (; iter != end; iter++) {
    if (a->count(*iter) == 0) {
      return false;
    }
  }
  return true;
}

/**
 * this function is to merge set a & b and put the results into a
 * the key thing here is we delete the repeated elements in both a & b, i.e.,
 * if e1 appears both in a & b, it will not appear in the result set.
 */
template <typename T> void uniqueSetMerge(set<T> *a, set<T> *b) {
  typename set<T>::iterator bIter = b->begin(), bEnd = b->end();
  for (; bIter != bEnd; bIter++) {
    T elemt = *bIter;

    if (a->count(elemt) == 0) {
      a->insert(elemt);
    } else {
      a->erase(elemt);
    }
  }
}

template <typename T> void set2list(list<T> *a, set<T> *b) {
  if (a == NULL || b == NULL)
    return;

  typename set<T>::iterator iter = b->begin(), end = b->end();

  for (; iter != end; iter++) {
    a->push_back(*iter);
  }
}

template <typename T>
void setIntersection(set<T> *a, set<T> *b, set<T> *results) {
  if (a == NULL || b == NULL)
    return;

  typename set<T>::iterator iter = b->begin(), end = b->end();

  for (; iter != end; iter++) {
    if (a->count(*iter) != 0) {
      results->insert(*iter);
    }
  }
}

/////////////////////////////////////////////////////////////////////////////
///////////////////////////////Map Operation/////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

template <class Key, class T, class Comparator, class MapAllocator,
          class SetAllocator>
static void make_key_set(const std::map<Key, T, Comparator, MapAllocator> &map,
                         std::set<Key, Comparator, SetAllocator> &set) {
  set.clear();
  typedef typename std::map<Key, T, Comparator, MapAllocator> map_type;
  typename map_type::const_iterator itr = map.begin();
  while (map.end() != itr) {
    set.insert((itr++)->first);
  }
}

/////////////////////////////////////////////////////////////////////////////
///////////////////////////////String Operation//////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// trim from beginning
static inline std::string &ltrim(std::string &s) {
  s.erase(s.begin(),
          std::find_if(s.begin(), s.end(),
                       std::not1(std::ptr_fun<int, int>(std::isspace))));
  return s;
}

// trim from end
static inline std::string &rtrim(std::string &s) {
  s.erase(std::find_if(s.rbegin(), s.rend(),
                       std::not1(std::ptr_fun<int, int>(std::isspace)))
              .base(),
          s.end());
  return s;
}

// trim from both ends
static inline std::string &trim(std::string &s) { return ltrim(rtrim(s)); }

template <typename T> static inline std::string num2string(T x) {
  ostringstream convert; // stream used for the conversion
  convert << x;          // insert the textual representation of 'Number' in the
                         // characters in the stream
  return convert.str();
}

static inline std::string char2string(char x) {
  stringstream ss;
  string s;
  ss << x;
  ss >> s;
  return s;
}

bool isNumeric(const std::string &s);

string int2string(int64_t number);

string idx2string(int64_t idx);

/////////////////////////////////////////////////////////////////////////////
////////////////////////////////////MISC/////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

string getDemangledName(string orgName);

void prettyTimePrint(time_t second);

/**
 * Here we use the heuristic on struct name
 * to filter some of them which has name like
 * anon, Msg, buf, etc
 *
 * if we do not need it, just make it always return true
 */
bool keywordMatch(string structname, list<string> *keywords);

#endif
