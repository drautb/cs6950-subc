int main(int argc, char* argv[])
{
  1 < 1;
  1 <= 1;
  1 == 1;
  1 > 1;
  1 >= 1;
  1 != 1;

  1 == 1 || 1 == 1;
  1 == 1 && 1 == 1;

  1 == 1 || 1 == 1 && 1 == 1;

  !(1 == 1 && 1 == 1);

  return 0;
}

@

((FunctionDefinition
  Int "main"
  (ArgList ((Int "argc" false)
            ((Pointer Char) "argv" true)))
  (Block
   ()
   ((Expression (LessThan (IntConst 1) (IntConst 1)))
    (Expression (LessThanEqual (IntConst 1) (IntConst 1)))
    (Expression (Equal (IntConst 1) (IntConst 1)))
    (Expression (GreaterThan (IntConst 1) (IntConst 1)))
    (Expression (GreaterThanEqual (IntConst 1) (IntConst 1)))
    (Expression (NotEqual (IntConst 1) (IntConst 1)))
    (Expression (LogicalOr
                 (Equal (IntConst 1) (IntConst 1))
                 (Equal (IntConst 1) (IntConst 1))))
    (Expression (LogicalAnd
                 (Equal (IntConst 1) (IntConst 1))
                 (Equal (IntConst 1) (IntConst 1))))
    (Expression (LogicalOr
                 (Equal (IntConst 1) (IntConst 1))
                 (LogicalAnd
                  (Equal (IntConst 1) (IntConst 1))
                  (Equal (IntConst 1) (IntConst 1)))))
    (Expression (LogicalNot
                 (LogicalAnd
                  (Equal (IntConst 1) (IntConst 1))
                  (Equal (IntConst 1) (IntConst 1)))))
    (Return ((IntConst 0)))))))
