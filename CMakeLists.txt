cmake_minimum_required(VERSION 3.10)
project(Trashscope)

set(CMAKE_CXX_STANDARD 20)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

# Get the necessary flags from llvm-config
execute_process(
  COMMAND llvm-config --cxxflags
  OUTPUT_VARIABLE LLVM_CXX_FLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND llvm-config --ldflags --system-libs --libs core
  OUTPUT_VARIABLE LLVM_LD_FLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Add these flags to your project
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LLVM_CXX_FLAGS}")
string(REPLACE "\n" " " LLVM_LD_FLAGS "${LLVM_LD_FLAGS}") # Ensure no newlines
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LLVM_LD_FLAGS}")

# Define the sources
add_executable(interactive Trashscope.cpp AST.cpp errors.cpp Tokenizer.cpp)

# Link against LLVM libraries (if needed)
llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(interactive ${llvm_libs})
