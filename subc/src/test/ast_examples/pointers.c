int main(int argc, char* argv[]) {
  char c;
  char *pc;
  char other_c;

  c = 'c';
  pc = &c;
  other_c = *pc;

  return 0;
}

@

((FunctionDefinition
  Int "main"
  (ArgList ((Int "argc" false)
            ((Pointer Char) "argv" true)))
  (Block
   ((Variable Char "c")
    (Variable (Pointer Char) "pc")
    (Variable Char "other_c"))
   ((Expression (Assignment (Id "c") (CharConst c)))
    (Expression (Assignment (Id "pc") (AddressOf (Id "c"))))
    (Expression (Assignment (Id "other_c") (Dereference (Id "pc"))))
    (Return ((IntConst 0)))))))
