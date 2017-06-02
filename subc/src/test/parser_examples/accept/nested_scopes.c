int main(int argc, char* argv[])
{
  int a;
  a = 5;
  {
    int a;
    a = 2;
    {
      return a + a;
    }
  }
}
