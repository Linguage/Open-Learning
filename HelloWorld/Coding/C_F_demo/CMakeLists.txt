cmake_minimum_required(VERSION 3.10)
project(CppFortranDemo)

# 编译 Fortran 文件
enable_language(Fortran)

# 添加可执行文件
add_executable(out main.cpp calculate.f90)
