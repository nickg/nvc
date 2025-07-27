module struct1;
  struct {
    int  a;
    byte b;
  } s1;

  typedef struct {
    int x, y;
  } t_pair;

  struct {
    int a;
    int a;   // Error
  } s2;
endmodule // struct1

