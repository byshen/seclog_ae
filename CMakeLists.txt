message(STATUS "Build type: ${LLVM_VERSION_MAJOR}")


add_llvm_library(AceInstrumentPass MODULE
    AceInstrument.cpp
    Utils.cpp

    SHARED
    ${LLVM_MAIN_INCLUDE_DIR}/llvm/Support

    PLUGIN_TOOL
    opt
)

set_target_properties(AceInstrumentPass PROPERTIES COMPILE_OPTIONS
    "-D__GLIBCXX_USE_CXX11_ABI=0 -fno-rtti" )

target_link_libraries(AceInstrumentPass)


