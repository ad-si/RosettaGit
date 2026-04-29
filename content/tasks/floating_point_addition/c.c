#include <stdio.h>

int main(void) {
    double a = 0.1;
    double b = 0.2;
    double sum = a + b;

    printf("%.17f\n", sum);          /* 0.30000000000000004 */
    printf("%s\n", sum == 0.3 ? "equal" : "not equal");
    return 0;
}
