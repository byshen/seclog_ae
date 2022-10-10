message(STATUS "Build type: ${LLVM_VERSION_MAJOR}")



add_subdirectory(dg)

add_llvm_library(AceInstrumentPass MODULE
    AceInstrument.cpp
    Utils.cpp

    DEPENDS 
    dgllvmdg

    PLUGIN_TOOL
    opt
)

target_link_libraries(AceInstrumentPass
					    PRIVATE dgllvmdg
					    )
set_target_properties(AceInstrumentPass PROPERTIES COMPILE_FLAGS
    "-D__GLIBCXX_USE_CXX11_ABI=0 -fno-rtti" )

