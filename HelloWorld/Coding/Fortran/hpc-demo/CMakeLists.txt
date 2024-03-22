cmake_minimum_required(VERSION 3.10)
set(CMAKE_Fortran_COMPILER "C:/Program Files (x86)/ntel/oneAPI/compiler/2024.0/bin/ifort.exe")

project(Fortran_OpenMP_MKL_Demo Fortran)

# Find MKL
find_package(MKL REQUIRED)

# Add executable
add_executable(main main.f90 mkl_demo.f90)

# Add OpenMP support
find_package(OpenMP REQUIRED)
if(OpenMP_Fortran_FOUND)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()

# Add MKL include directories and link libraries
include_directories(${MKL_INCLUDE_DIRS})
target_link_libraries(main PRIVATE ${MKL_LIBRARIES})