int main(int argc, char* argv[]) {
  int x;
  int y;
  x = 42;

  if (x < 20) {
    y = 20;
  }
  else if (x > 50) {
    y = 50;
  }
  else {
    y = 42;
  }

  return y;
}