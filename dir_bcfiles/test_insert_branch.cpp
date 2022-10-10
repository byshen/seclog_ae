#include <stdio.h>
#include <stdlib.h>

void ace_printf(const char *format, ...) {
  char dest[1024 * 16];
  va_list argptr;
  va_start(argptr, format);
  vsprintf(dest, format, argptr);
  va_end(argptr);
  printf("%s", dest);
}

// int EQ 0
int result_check_func_1(int res) {
    return int(res == 0);
}

// int NE 0
int result_check_func_2(int res) {
    return int(res != 0);
}

// pointer EQ 4399
// int result_check_func_3(YOURTYPEHERE *res) {
//     return int(res == NULL);
// }

// pointer NE 4399
int result_check_func_3(void *res) {
    if (res == NULL) return 1;
    return 0;
}





int
vsf_access_check_number(int * a, int b){
    if (*a + b == 2){
        return 0;
    }
    return 1;
} 

int main () {
    /*dummy functions to add them into call graph*/
    result_check_func_1(0);

    int a = 1;
    int b = 1;
    //return 0 is deny
    vsf_access_check_number(&a , b);
    return 0;
}
