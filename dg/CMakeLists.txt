OPTION(LLVM_DG "Support for LLVM Dependency graph" ON)
OPTION(ENABLE_CFG "Add support for CFG edges to the graph" ON)
set(LLVM_INCLUDE_DIRS "/usr/local/clang_bk/include")
set(LLVM_LIBRARY_DIRS "/usr/local/clang_bk/lib")
set(LLVM_DEFINITIONS "-D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS")


OPTION(LLVM_DG "Support for LLVM Dependency graph" ON)
OPTION(ENABLE_CFG "Add support for CFG edges to the graph" ON)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

if (LLVM_DG)
	# for llvm dg we need cfg and postdom edges
	if (NOT ENABLE_CFG)
		message(STATUS "Enabling CFG edges due to llvm dg")
	endif()

	set(ENABLE_CFG ON)

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

	# if we were provided a path to custom sources
	# use that path
	if (LLVM_SRC_PATH)
		include_directories(${LLVM_SRC_PATH}/include)
		message(STATUS "Looking for headers in given: ${LLVM_SRC_PATH}/include")
	else()
		include_directories(${LLVM_INCLUDE_DIRS})
		message(STATUS "Looking for headers in: ${LLVM_INCLUDE_DIRS}")
	endif()

	# if we were provided a path to custom build directory
	# use that
	if (LLVM_BUILD_PATH)
		link_directories(${LLVM_BUILD_PATH}/lib)

		# llvm-config.h
		include_directories("${LLVM_BUILD_PATH}/include")
		message(STATUS "Looking for libraries in given: ${LLVM_BUILD_PATH}/lib")
	else()
		link_directories(${LLVM_LIBRARY_DIRS})
		message(STATUS "Looking for libraries in: ${LLVM_LIBRARY_DIRS}")
	endif(LLVM_BUILD_PATH)

	add_definitions(${LLVM_DEFINITIONS})
	add_definitions(-DHAVE_LLVM)


endif(LLVM_DG)

llvm_map_components_to_libnames(llvm_libs support core irreader bitwriter analysis)


add_library(dganalysis SHARED
	Offset.h
	ADT/DGContainer.h
	ADT/Bitvector.h
	ADT/Bits.h
	ADT/NumberSet.h

	Offset.cpp
	Debug.cpp
	BBlockBase.cpp
)

add_library(dgpta SHARED
	SubgraphNode.h
	PointerAnalysis/Pointer.h
	PointerAnalysis/PointsToSet.h
	PointerAnalysis/MemoryObject.h
	PointerAnalysis/PointerGraph.h
	PointerAnalysis/PointerAnalysis.h
	PointerAnalysis/PointerAnalysisFI.h
	PointerAnalysis/PointerAnalysisFS.h
	PointerAnalysis/PointerGraphValidator.h

	PointerAnalysis/Pointer.cpp
	PointerAnalysis/PointerAnalysis.cpp
	PointerAnalysis/PointerGraphValidator.cpp
	PointerAnalysis/PointsToSet.cpp
)
target_link_libraries(dgpta PUBLIC dganalysis)

add_library(dgdda SHARED
	ReadWriteGraph/RWNode.h
	ReadWriteGraph/RWBBlock.h
	ReadWriteGraph/ReadWriteGraph.h
	DataDependence/DataDependenceAnalysisImpl.h
	DataDependence/DataDependence.h
	MemorySSA/MemorySSA.h
	MemorySSA/ModRef.h
	MemorySSA/Definitions.h

	ReadWriteGraph/ReadWriteGraph.cpp
	MemorySSA/MemorySSA.cpp
        MemorySSA/ModRef.cpp
        MemorySSA/Definitions.cpp
)
target_link_libraries(dgdda PUBLIC dganalysis)


if (LLVM_DG)

add_library(dgllvmforkjoin SHARED
	llvm/ForkJoin/ForkJoin.h
	llvm/ForkJoin/ForkJoin.cpp
)

target_link_libraries(dgllvmforkjoin PRIVATE dgllvmpta)


add_library(dgllvmpta SHARED
	llvm/PointerAnalysis/PointerAnalysis.h
	llvm/PointerAnalysis/LLVMPointerAnalysisOptions.h
	llvm/PointerAnalysis/PointerGraph.h

	llvm/PointerAnalysis/PointerGraphValidator.h
	llvm/PointerAnalysis/PointerAnalysis.cpp
	llvm/PointerAnalysis/PointerGraph.cpp
	llvm/PointerAnalysis/PointerGraphValidator.cpp
	llvm/PointerAnalysis/Block.cpp
	llvm/PointerAnalysis/Interprocedural.cpp
	llvm/PointerAnalysis/Structure.cpp
	llvm/PointerAnalysis/Globals.cpp
	llvm/PointerAnalysis/Constants.cpp
	llvm/PointerAnalysis/Instructions.cpp
	llvm/PointerAnalysis/Calls.cpp
	llvm/PointerAnalysis/Threads.cpp
)
target_link_libraries(dgllvmpta PUBLIC dgpta ${llvm_libs})


add_library(dgllvmdda SHARED
	llvm/ReadWriteGraph/LLVMReadWriteGraphBuilder.cpp
	llvm/ReadWriteGraph/Instructions.cpp
	llvm/ReadWriteGraph/Calls.cpp
	llvm/DataDependenceAnalysis/LLVMDataDependenceAnalysis.cpp
	llvm/ForkJoin/ForkJoin.h
	llvm/ForkJoin/ForkJoin.cpp

	llvm/DataDependence/DataDependence.h
	llvm/ReadWriteGraph/LLVMReadWriteGraphBuilder.h
)
target_link_libraries(dgllvmdda
			PUBLIC dgllvmpta
			PUBLIC dgdda
			PUBLIC dgllvmforkjoin
			PUBLIC ${llvm_libs})

add_library(dgllvmthreadregions SHARED "")
include(${CMAKE_CURRENT_SOURCE_DIR}/llvm/ThreadRegions/CMakeLists.txt)
target_include_directories(dgllvmthreadregions
        PUBLIC
            llvm/ThreadRegions)
target_link_libraries(dgllvmthreadregions PUBLIC dgllvmpta
                                          PRIVATE dgllvmforkjoin
				PRIVATE ${llvm_libs}
				)

add_library(dgllvmcda SHARED
            llvm/ControlDependence/Block.cpp
            llvm/ControlDependence/Function.cpp
            llvm/ControlDependence/GraphBuilder.cpp
            llvm/ControlDependence/ControlDependence.cpp
            llvm/ControlDependence/InterproceduralCD.cpp
            llvm/ControlDependence/InterproceduralCD.h
            llvm/ControlDependence/NTSCD.cpp
            llvm/ControlDependence/NTSCD.h
            llvm/ControlDependence/SCD.cpp
            llvm/ControlDependence/SCD.h
            llvm/ControlDependence/ControlDependence.h
            llvm/ControlDependence/LLVMControlDependenceAnalysisImpl.h
            )
target_link_libraries(dgllvmcda INTERFACE dgllvmpta
				INTERFACE ${llvm_analysis} 
                                PRIVATE dgllvmforkjoin 
								PRIVATE dgllvmpta
								PUBLIC ${llvm_libs}
							)
		

add_library(dgllvmdg SHARED
	BBlock.h
	Node.h
	DependenceGraph.h
	llvm/LLVMNode.h
	llvm/LLVMDependenceGraph.h
	llvm/LLVMDependenceGraphBuilder.h
	llvm/LLVMSlicer.h

	llvm/LLVMDGVerifier.h
	llvm/llvm-utils.h

	llvm/LLVMNode.cpp
	llvm/LLVMDependenceGraph.cpp
	llvm/LLVMDGVerifier.cpp
	llvm/Dominators/PostDominators.cpp
	llvm/DefUse/DefUse.cpp
	llvm/DefUse/DefUse.h
)

# Get proper shared-library behavior (where symbols are not necessarily
# resolved when the shared library is linked) on OS X.
if(APPLE)
  set_target_properties(dgllvmdg PROPERTIES
    LINK_FLAGS "-undefined dynamic_lookup"
  )

  set_target_properties(dgllvmsdg PROPERTIES
    LINK_FLAGS "-undefined dynamic_lookup"
  )

  set_target_properties(dgllvmpta PROPERTIES
    LINK_FLAGS "-undefined dynamic_lookup"
  )

  set_target_properties(dgllvmdda PROPERTIES
    LINK_FLAGS "-undefined dynamic_lookup"
  )

  set_target_properties(dgllvmcda PROPERTIES
    LINK_FLAGS "-undefined dynamic_lookup"
  )
endif(APPLE)

if (APPLE)
	target_link_libraries(dgllvmdg
				PUBLIC dgllvmpta
				PUBLIC dgllvmdda
				PUBLIC dgllvmthreadregions
				PUBLIC dgllvmcda
				PRIVATE ${llvm_libs}
				)
else()
	target_link_libraries(dgllvmdg
				PUBLIC dgllvmpta
				PUBLIC dgllvmdda
				PUBLIC dgllvmthreadregions
				PUBLIC dgllvmcda)
endif(APPLE)

add_library(dgsdg SHARED
	SystemDependenceGraph/DependenceGraph.cpp

	#llvm/SystemDependenceGraph/SystemDependenceGraph.h
	SystemDependenceGraph/SystemDependenceGraph.h
	SystemDependenceGraph/DependenceGraph.h
	SystemDependenceGraph/DGElement.h
        SystemDependenceGraph/DepDGElement.h
        SystemDependenceGraph/DGNode.h
	SystemDependenceGraph/DGArgumentPair.h
	SystemDependenceGraph/DGBBlock.h
)
target_link_libraries(dgsdg
		      PUBLIC dgpta
		      PUBLIC dgdda)

add_library(dgllvmsdg SHARED
	llvm/SystemDependenceGraph/SystemDependenceGraph.cpp
	llvm/SystemDependenceGraph/Dependencies.cpp

	llvm/SystemDependenceGraph/SystemDependenceGraph.h
)
target_link_libraries(dgllvmsdg
		      PUBLIC dgsdg
		      PUBLIC dgllvmpta
		      PUBLIC dgllvmdda
		      PUBLIC dgllvmcda)

install(TARGETS dgllvmdg dgllvmthreadregions dgllvmcda
                dgllvmpta dgllvmdda dgpta dgdda dganalysis
		dgllvmforkjoin dgllvmsdg
	LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

endif(LLVM_DG)
