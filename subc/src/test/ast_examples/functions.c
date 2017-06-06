int sum(int n1, int n2);

int main(int argc, char* argv[])
{
  return sum(-5, 5);
}

int sum(int n1, int n2) {
  return n1 + n2;
}

@

((Declaration (FunctionDeclaration
               Int "sum"
               (ArgList ((Int "n1" false)
                         (Int "n2" false)))))
 (FunctionDefinition
  Int "main"
  (ArgList ((Int "argc" false)
            ((Pointer Char) "argv" true)))
  (Block
   ()
   ((Return
     ((FunctionCall
       (Id "sum")
       ((Negate (IntConst 5))
        (IntConst 5))))))))
 (FunctionDefinition
  Int "sum"
  (ArgList ((Int "n1" false)
            (Int "n2" false)))
  (Block
   ()
   ((Return
     ((Add (Id "n1") (Id "n2"))))))))
