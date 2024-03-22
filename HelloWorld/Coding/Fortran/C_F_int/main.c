#include <stdio.h>

// Fortran 声明
void calculate_(double *x, double *y, double *z);

int main() {
    double x = 3.0, y = 4.0, z;
    
    // 调用 Fortran 函数
    calculate_(&x, &y, &z);
    
    printf("The result is: %f\n", z);
    
    return 0;
}
