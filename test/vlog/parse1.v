module parse1;
  wire [8:0] x, y, z;
  reg        zz;
  assign y = x;
  always begin : foo
    $display("hello");
    $finish;
    if (x);
    if (x) z <= 1;
    if (x) $display("yes");
    else if (z);
    else $display("no");
    z = 0;
    z = ~x;
    z = !x;
  end
  assign x = x | y;
  pulldown (supply0) p1 (x);
  pulldown p2 (x);
  pullup p3 (y);
  pullup (supply0, supply1) p4 (y);
  pullup (y);
  always @(x or y or (posedge z)) begin
  end
  initial begin
    while (z);
    repeat (x) x = ~x;
    do x = y; while (1);
  end
  initial begin
    x = x << 1;
    x = x >> 1;
    x = y <<< x;
    x = x >>> y;
  end
  int r1 = 5;
  wire w1 = 0;
  task task1;
    input int x = 6;
    bit       y;
    @(x) y = 2;
  endtask // task1
  task task2(input int x, inout logic, output logic y);
    y = !x;
  endtask // task2
  function sum(input int a, b);
    sum = a + b;
  endfunction // sum
  initial begin
    wait (x) y = 1;
    x = x ? 1 : y;
    x = x - y * x + 1 % y / x;
    x = x ** y;
    task2(1, x, y);
  end
  parameter xx = 6;
  integer   r2 = 66;
  real      r3 = 1.0;
  shortreal r4 = 6.7;
  realtime  r5 = 1.0;
  assign r2 = {x, y};
  assign {r1, r2} = {x, y};
  initial begin
    for (r1 = 1; r1 < 5; r1++)
      x = x + 1;
    for (;;r2 = r2 * x);
    for (int i = 0; i > 0; --i);
    for (var reg x = 5;;);
  end
  localparam [7:0] yy = 5;
  localparam bit zzz = 0;
  wire [y-1:0]  w2;
  reg [7:0]     array1 [31:0];
  wire [3:0]    array2 [6:0];
  assign w3 = &(y) | ^(|y);
  event e;
  always_comb x = 1;
  always_ff x = 2;
  always_latch begin
    x = 3;
  end
  always @(w2) ->e;
  always @(  *) x <= y;
  initial
    case (x)
      2'b00: y = 1;
      2'b01, 2'b10: ;
      default: y = 2;
    endcase // case (x)
  mymod #(.x(1),.y(2)) m1(), m2(.foo(5));
  mymod #(1+2) m3(x);
  myudp (1);
  buf (x, 1'b0);
  initial x[1:0] = y[7+:6];
  generate if (y == 7) begin
    mod1 m4(x);
  end else begin
    mod1 m5(x);
  end endgenerate
  assign x = sum(y, z);
  function automatic real min(input real a, input real b);
    if (a < b) min = a;
    else min = b;
  endfunction // min
  time t1;
  initial begin
    x = y ^ z;
    x = y ~^ z;
    x = y ^~ z;
  end
  triand w4;
  trior  w5;
  (* foo=1 *) tri w6 = w5;
  uwire  w7;
  assign x = array1[array2[y]];
  initial fork
    #1 x = 1;
    #5 y = 2;
  join
  genvar i;
  for (i = 0; i < 5; i++)
    assign x[i] = 1;
endmodule // parse1
