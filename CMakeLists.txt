message(STATUS "Build type: ${LLVM_VERSION_MAJOR}")

set(LLVM_INCLUDE_DIRS "/usr/local/clang_9.0.0/include")
set(LLVM_LIBRARY_DIRS "/usr/local/clang_9.0.0/lib")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "LLVM binaries: ${LLVM_TOOLS_BINARY_DIR}")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${LLVM_DIR}")
include(LLVMConfig)
include(AddLLVM)

message(STATUS "LLVM include dir: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM libraries dir: ${LLVM_LIBRARY_DIRS}")
message(STATUS "LLVM definitions: ${LLVM_DEFINITIONS}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
add_definitions(-DHAVE_LLVM)

llvm_map_components_to_libnames(llvm_libs support core irreader bitwriter analysis)


add_llvm_library(AceInstrumentPass MODULE
    AceInstrument.cpp
    Utils.cpp

    DEPENDS
    intrinsics_gen

    PLUGIN_TOOL
    opt
)

set_target_properties(AceInstrumentPass PROPERTIES COMPILE_OPTIONS
    "-D__GLIBCXX_USE_CXX11_ABI=0 -fno-rtti" )

target_link_libraries(AceInstrumentPass PUBLIC ${llvm_libs})


