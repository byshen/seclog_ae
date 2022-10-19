message(STATUS "Build type: ${LLVM_VERSION_MAJOR}")



# add_subdirectory(dg)    
#   DEPENDS 
#   dgllvmdg
# 	PRIVATE dgllvmdg

add_llvm_library(AceInstrumentPass MODULE
    AceInstrument.cpp
    Utils.cpp

    SHARED
    ${LLVM_MAIN_INCLUDE_DIR}/llvm/Support
    
    PLUGIN_TOOL
    opt
)

target_link_libraries(AceInstrumentPass

					    )
set_target_properties(AceInstrumentPass PROPERTIES COMPILE_FLAGS
    "-D__GLIBCXX_USE_CXX11_ABI=0 -fno-rtti" )

