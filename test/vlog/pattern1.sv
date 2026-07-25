module pattern1;
  reg [7:0] x;
  typedef struct {
    int f1;
    real f2;
  } t_s1;
  t_s1 s1;

  initial x = '{default:(0)};   // OK
  initial s1 = '{f1: 5, f2: 5.1};  // OK
  initial s1 = '{f1: 5, bogus: 0};  // Error
  initial x = '{f1: 5, f2: 5.1};  // Error
  initial force x = '{1: 0, default:1};  // OK
endmodule // pattern1
