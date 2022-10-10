#ifndef DG_LLVM_CDA_OPTIONS_H_
#define DG_LLVM_CDA_OPTIONS_H_

#include "../LLVMAnalysisOptions.h"
#include "../../ControlDependence/ControlDependenceAnalysisOptions.h"

namespace dg {

struct LLVMControlDependenceAnalysisOptions :
    public LLVMAnalysisOptions, ControlDependenceAnalysisOptions
{
};

} // namespace dg

#endif
