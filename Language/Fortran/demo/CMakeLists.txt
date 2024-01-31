cmake_minimum_required(VERSION 3.0)
project(FortranDemo)

# 设置Fortran编译器
enable_language(Fortran)

# 添加可执行文件
add_executable(demo src/main.f90 src/calculations.f90 src/output.f90)
