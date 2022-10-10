include_directories(${CMAKE_CURRENT_LIST_DIR}/../../include/)

message(STATUS "CMAKE_CURRENT_LIST_DIR ${CMAKE_CURRENT_LIST_DIR}")
include(${CMAKE_CURRENT_LIST_DIR}/Nodes/CMakeLists.txt)
include(${CMAKE_CURRENT_LIST_DIR}/Graphs/CMakeLists.txt)

target_sources(dgllvmthreadregions
		PRIVATE
			${CMAKE_CURRENT_LIST_DIR}/ThreadRegion.cpp
                        ${CMAKE_CURRENT_LIST_DIR}/MayHappenInParallel.cpp)
