int main(int argc, char* argv[])
{
  int x;
  x = 0;

  while (x < 10) {
    x = x + 2;
  }

  if (x == 10) {
    return 0;
  }
  else {
    return 1;
  }
}

@

((FunctionDefinition
  Int "main"
  (ArgList ((Int "argc" false)
            ((Pointer Char) "argv" true)))
  (Block
   ((VariableDeclaration Int "x"))
   ((Expression (Assignment (Id "x") (IntConst 0)))
    (Loop
     (LessThan (Id "x") (IntConst 10))
     (Block
      ()
      ((Expression (Assignment (Id "x") (Add (Id "x") (IntConst 2)))))))
    (Conditional
     (Equal (Id "x") (IntConst 10))
     (Block
      ()
      ((Return ((IntConst 0)))))
     ((Block
       ()
       ((Return ((IntConst 1)))))))))))
