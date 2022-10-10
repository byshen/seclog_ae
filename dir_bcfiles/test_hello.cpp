#include <iostream>
using namespace std;

int greater_than_zero(int i) {
    if (i>0) 
        return 1;
    return 0;
}

int test(int x) {
    int retval = -1;
    if (x > -10) {
        retval = greater_than_zero(x);
    }

    if (!retval) { // check failed
        return 1;
    }

    return 0;
}
int main () {
     
    cout << "Hello World" <<endl;
    
    test(100);
    return 0;
}
