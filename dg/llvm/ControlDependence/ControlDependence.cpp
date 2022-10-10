#include "ControlDependence.h"
#include "NTSCD.h"
#include "SCD.h"
#include "InterproceduralCD.h"

namespace dg {

void LLVMControlDependenceAnalysis::initializeImpl() {
    if (getOptions().standardCD()) {
        _impl.reset(new llvmdg::SCD(_module, _options));
    } else if (getOptions().ntscdCD()) {
        _impl.reset(new llvmdg::NTSCD(_module, _options));
    } else {
        assert(false && "Unhandled analysis type");
        abort();
    }

    _interprocImpl.reset(new llvmdg::LLVMInterprocCD(_module, _options));
}

} // namespace dg
