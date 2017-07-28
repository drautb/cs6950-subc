int fib(int n);

int fib(int n) {
  if (n < 2) {
    return n;
  }

  return fib(n - 2) + fib(n - 1);
}

int main(int argc, char* argv[]) {
  return fib(10);
}
