#include <iostream>
using namespace std;

int greater_than_zero(int i) {
    if (i>0) 
        return 1;
    return 0;
}

int exit_func() {
    exit(1);
}

#define large_non_sense(x) ({\
    (x)=(((23)*x)-x);\
    (x)=(x+2);\
})

int test(int x) {
    int retval = -1;
    if (x > -10) {
        retval = greater_than_zero(x);
    }

    if (!retval) { // check failed
        exit_func();
    }

    large_non_sense(x);
    return 0;
}
int main () {
     
    cout << "Hello World" <<endl;
    
    test(100);
    return 0;
}
