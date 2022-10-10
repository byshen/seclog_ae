#include <iostream>
using namespace std;

#define APR_SUCCESS 0
int greater_than_zero(int i) {
    if (i>0) 
        return 1;
    return 0;
}

void my_log_error() {
    cout<< "ERROR\n";
}

bool greater_than_zero_bool(int i) {
    if (i>0) {
        my_log_error();
        return true;
    }
    return false;
}

int get_other(int x) {
    if (x > 10)
        return x;
    return 0;
}

int util_is_error(int x) {
    if (x !=APR_SUCCESS) 
        return 1;
    return 0;
}

int test(int x) {
    int retval = -1;
    if (x > -10) {
        retval = greater_than_zero(x);
    }

    if (util_is_error(retval)) { // check failed
        return 1;
    }

    bool tmp = greater_than_zero_bool(x);
    if (tmp) {
        my_log_error();
        return 1;
    }
    retval = get_other(x);

    if (retval != APR_SUCCESS) { // check failed
        return 1;
    }
    
    int arr[] = {0, 0};
    int y = arr[retval];

    return y;
}
int main () {
     
    cout << "Hello World" <<endl;
    
    test(100);
    return 0;
}
