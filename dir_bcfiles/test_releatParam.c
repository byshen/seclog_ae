#include <stdio.h>
#include <stdlib.h>
#include <time.h>


int processA(int a){
    if (a%3 == 0){
        return 0;
    }
    return 1;
}

int processB(int a){
    if (a%4 == 0){
        return 0;
    }
    return 1;
}

int processC(int a){
    if (a%5 == 0){
        return 0;
    }
    return 1;
}

// int 
// vsf_access_check_number(int * a, int b){
//     // if (b == 0) {
//     //     return 0;
//     // }
//     if (processA(* a)){
//         * a = * a +1;
//     }
//     if (processB(b)){
//         return 0;
//     }
//     if (processC(* a)){
//         return 0;
//     }
    
//     return 1;

// } 

int 
vsf_access_check_number(int * a, int b){
    // if (b == 0) {
    //     return 0;
    // }
    int d = 1;
    if (processA(* a)){
        d = 0;
    }
    if (processB(b)){
        d =0; 
    }
    if (processC(* a)){
       d=0;
    }
    
    return d;

} 
int main () {
    srand(time(0));
    int a = rand();
    int b = rand();
    //return 0 is deny
    if (!vsf_access_check_number(&a , b)){
        printf("Access denied\n");
        return 1;
    }
    return 0;
}
