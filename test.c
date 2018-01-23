#include <stdio.h>

int scheme_entry(int n) {
    return n;
}

int main(int argc, char ** argv) {
    printf("%d\n", scheme_entry(42));
}