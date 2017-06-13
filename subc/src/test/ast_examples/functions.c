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
               (ArgList ((Int "n1")
                         (Int "n2")))))
 (FunctionDefinition
  Int "main"
  (ArgList ((Int "argc")
            ((Array (Pointer Char)) "argv")))
  (Block
   ()
   ((Return
     ((FunctionCall
       (Id "sum")
       ((Negate (IntConst 5))
        (IntConst 5))))))))
 (FunctionDefinition
  Int "sum"
  (ArgList ((Int "n1")
            (Int "n2")))
  (Block
   ()
   ((Return
     ((Add (Id "n1") (Id "n2"))))))))
