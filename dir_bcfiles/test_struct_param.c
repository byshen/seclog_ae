#include <stdio.h>
#include <stdlib.h>
#include <time.h>


char msg[4] = "new";
int METHOD = 200;
int g_intA = 1024;
int METHOD_POST=230;

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
    if (res == 0) return 1;
    return 0;
}

int processA(int a){
    if (a%3 == 0){
        return 0;
    }
    return 1;
}

int processB(int a){
    if (a%4 == 0){
        return 1;
    }
    return 0;
}

int processC(int a){
    if (a%5 == 0){
        return 0;
    }
    return 1;
}

void add_dep();

struct request {
    int method;
    char* method_name;
    int third; 
};

int is_method_error(struct request* r) {
    return r->method == 200;
}


int 
vsf_access_check_number(struct request* r, int b, char* msg){
    if (b == 0) {
        return 0;
    }

    if (processB(b)){
        printf("b=%d\n", g_intA);
        return 0;
    }else {
        if (r->third == METHOD_POST){
            if (msg != NULL) {
                printf("%s", msg);
            }
            return 0;
        }
    }
    return 1;
} 

int assign(int* a, int* b) {
    *b = *a;
    return 0;
}

int help_main (int b) {
    srand(time(0));
    // int a = rand();
    // b = METHOD;
    //return 0 is deny

    printf("hello world\n");

    struct request*  a = (struct request*) malloc(sizeof(struct request));
    
    a->method = METHOD;
    a->method_name = msg;
    a->third = 1;


    printf("%s", msg);

    if (!is_method_error(a)){
	return 0;

    }
    int local_b;
    assign(&b, &local_b); // This add dependency, so we can trace back to the parameter b;

    if (!vsf_access_check_number(a, local_b, msg)){
        printf("Access denied\n");
        return 1;
    }
    return 0;
}

int main() {
    result_check_func_1(0);
    ace_printf("ace");


    help_main(METHOD);
    return 0;
}
