#include <stdio.h>
#include <stdlib.h>

extern int fir_entry();

const int INT_TAG = 0x0;
const int BOOL_TAG = 0x00000001;
const int BOOL_TRUE = 0x80000000;
const int BOOL_FALSE = 0x0;

int print(int val) {
    if((val & BOOL_TAG)) {
        if(val == (BOOL_TRUE | BOOL_TAG)) {
            printf("#t");
        } else if(val == (BOOL_FALSE | BOOL_TAG)) {
            printf("#f");
        } else {
            printf("0x%x", val);
        }
    } else {
        printf("%d", val >> 1);
    }
    return val;
}

int print_int(int val) {
    printf("%d", val);
    return val;
}

void error(int val, int errno) {
    switch(errno) {
        default:
            fprintf(stderr, "unknown err");
    }
    exit(errno);
}

int add(int a, int b) {
    return a + b;
}

int gt(int a, int b) {
    return a > b;
}

int main() {
    return print_int(fir_entry());
    //return 0;
}
