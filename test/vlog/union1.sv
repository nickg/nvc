module union1;
  union { int x; logic y; } u1;
  typedef union { int a; logic b; } t_u2;

  typedef union packed {
    logic [3:0] bits;
    struct packed {
      logic [1:0] high;
      logic [1:0] low;
    } words;
  } t_u3;

  t_u3 u3;

  initial begin
    u1.x = 5;
    u1.y = 1'b1;
    u3.bits = 4'b1001;
    u3.words = 4'b1100;
  end
endmodule // union1
