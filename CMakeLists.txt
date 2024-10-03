cmake_minimum_required(VERSION 3.10)
project(Trashscope)

set(CMAKE_CXX_STANDARD 20)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

# Define the sources
add_executable(interactive Trashscope.cpp AST.cpp errors.cpp Tokenizer.cpp Optimizer.cpp)

# List the LLVM components your project depends on
set(LLVM_LINK_COMPONENTS
    core
    support
    irreader
    passes
    transformutils
    scalaropts
    analysis
    instcombine
    nativecodegen
    orcjit

    # Add the native target component
    ${LLVM_TARGETS_TO_BUILD}
    )

# Map the LLVM components to library names
llvm_map_components_to_libnames(llvm_libs ${LLVM_LINK_COMPONENTS})

# Link your executable against the LLVM libraries
target_link_libraries(interactive ${llvm_libs})
