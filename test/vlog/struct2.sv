// Semantic errors
module struct2;
  struct {
    int  a;
    byte b;
  } s1;

  typedef struct {
    int x, y;
  } t_pair;

  initial begin
    s1.a = 5;      // OK
    t_pair.x = 1;  // Error
  end
endmodule // struct2
