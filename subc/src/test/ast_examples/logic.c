int main(int argc, char* argv[])
{
  if (1 < 1) {}
  if (1 <= 1) {}
  if (1 == 1) {}
  if (1 > 1) {}
  if (1 >= 1) {}
  if (1 != 1) {}

  return 0;
}

@

((FunctionDefinition
  Int "main"
  (ArgList ((Int "argc" false)
            ((Pointer Char) "argv" true)))
  (Block
   ()
   ((Conditional (LessThan (IntConst 1) (IntConst 1)) (Block () ()) None)
    (Conditional (LessThanEqual (IntConst 1) (IntConst 1)) (Block () ()) None)
    (Conditional (Equal (IntConst 1) (IntConst 1)) (Block () ()) None)
    (Conditional (GreaterThan (IntConst 1) (IntConst 1)) (Block () ()) None)
    (Conditional (GreaterThanEqual (IntConst 1) (IntConst 1)) (Block () ()) None)
    (Conditional (NotEqual (IntConst 1) (IntConst 1)) (Block () ()) None)
    (Return ((IntConst 0)))))))
