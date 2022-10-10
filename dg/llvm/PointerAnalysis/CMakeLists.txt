add_llvm_library(dgllvmpta SHARED
	PointerAnalysis.h
	LLVMPointerAnalysisOptions.h
	PointerGraph.h

	PointerGraphValidator.h
	PointerAnalysis.cpp
	PointerGraph.cpp
	PointerGraphValidator.cpp
	Block.cpp
	Interprocedural.cpp
	Structure.cpp
	Globals.cpp
	Constants.cpp
	Instructions.cpp
	Calls.cpp
	Threads.cpp
)

target_link_libraries(dgllvmpta PUBLIC dgpta)

