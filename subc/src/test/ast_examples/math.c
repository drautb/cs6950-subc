int result;

int main(int argc, char* argv[])
{
  int a;
  int b;
  int c;
  int d;

  a = 2 + 2;
  b = a - 3;
  c = b * 4;
  d = c / 5;
  d = -d;

  return 0;
}

@

((Declaration (Variable Int "result"))
 (FunctionDefinition
  Int "main"
  (ArgList ((Int "argc" false)
            ((Pointer Char) "argv" true)))
  (Block
   ((Variable Int "a")
    (Variable Int "b")
    (Variable Int "c")
    (Variable Int "d"))
   ((Expression (Assignment (Id "a") (Add (IntConst 2) (IntConst 2))))
    (Expression (Assignment (Id "b") (Subtract (Id "a") (IntConst 3))))
    (Expression (Assignment (Id "c") (Multiply (Id "b") (IntConst 4))))
    (Expression (Assignment (Id "d") (Divide (Id "c") (IntConst 5))))
    (Expression (Assignment (Id "d") (Negate (Id "d"))))
    (Return ((IntConst 0)))))))
