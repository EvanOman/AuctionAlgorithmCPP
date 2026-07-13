cmake_minimum_required(VERSION 3.16)
project(auction VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

add_compile_options(-Wall -Wextra -Wpedantic -Werror)

# Core library: shared for distribution / FFI, static for embedding.
set(AUCTION_SOURCES src/auction.cpp src/problem.cpp)

add_library(auction_shared SHARED ${AUCTION_SOURCES})
add_library(auction_static STATIC ${AUCTION_SOURCES})
foreach(target auction_shared auction_static)
  target_include_directories(${target} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)
endforeach()
set_target_properties(auction_shared PROPERTIES
  OUTPUT_NAME auction
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR})
set_target_properties(auction_static PROPERTIES OUTPUT_NAME auction)

# CLI
add_executable(auction_cli apps/auction_cli.cpp)
set_target_properties(auction_cli PROPERTIES OUTPUT_NAME auction)
target_link_libraries(auction_cli PRIVATE auction_static)

# Tests
enable_testing()
add_executable(auction_tests
  tests/test_auction.cpp
  tests/test_problem.cpp
  tests/test_main.cpp)
target_include_directories(auction_tests PRIVATE tests/third_party)
target_link_libraries(auction_tests PRIVATE auction_static)
add_test(NAME auction_tests COMMAND auction_tests)

# Install rules for the distributable library + CLI.
include(GNUInstallDirs)
install(TARGETS auction_shared auction_static auction_cli
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(DIRECTORY include/auction DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
