int fib(int n, int current, int next);

int fib(int n, int current, int next) {
  if (n == 0) {
    return current;
  }
  else {
    return fib(n - 1, next, current + next);
  }
}

int main(int argc, char* argv[]) {
  return fib(10, 0, 1);
}
