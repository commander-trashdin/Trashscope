#include <iostream>

extern "C" {
double fib(double);
}

int main() { std::cout << "fib no 10: " << fib(10.0) << std::endl; }