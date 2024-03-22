#include <iostream>

// 声明 Fortran 中的函数
extern "C" {
    void calculate_(double* x, double* y, double* z);
}

int main() {
    double x = 5.0, y = 3.0, z;

    // 调用 Fortran 函数
    calculate_(&x, &y, &z);

    std::cout << "Result from Fortran: " << z << std::endl;

    return 0;
}
