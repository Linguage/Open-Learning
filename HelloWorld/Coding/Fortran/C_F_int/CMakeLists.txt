cmake_minimum_required(VERSION 3.10)
project(MixedLanguageDemo LANGUAGES C Fortran)

# 添加 C 源文件
add_executable(MixedLanguageDemo main.c)

# 添加 Fortran 源文件
enable_language(Fortran)
add_library(FortranLibrary calculate.f90)

# 将 Fortran 目标链接到 C 目标
target_link_libraries(MixedLanguageDemo PRIVATE FortranLibrary)
