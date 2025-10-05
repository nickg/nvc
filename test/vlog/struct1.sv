// Parse errors
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

  initial begin
    s1.a = 5;      // OK
    s1.c = 7;      // Error
    s1.a.x = 2;    // Error
  end
endmodule // struct1
