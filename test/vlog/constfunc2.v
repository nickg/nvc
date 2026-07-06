module constfunc2;
  localparam p0 = 2 + 0;
  localparam p1 = func1(2, 3, 2'b10);
  parameter  p2 = 2;
  localparam p3 = func2(5);
  localparam p4 = func3(5);

  function [p0-1:0] add_zero;
    input [p0-1:0] x;
    add_zero = x + 0;
  endfunction // add_zero

  function [(p0+1+1-2)*p0-1:0] func1;
    input integer x, y;
    input [p0-1:0] pat;
    func1 = {(p0+1+1-2){add_zero(pat)}};
  endfunction // func1

  function integer func2;
    input integer x;
    func2 = x + p2;
  endfunction // func2

  function integer func3;
    input integer x;
    func3 = x + p4;
  endfunction // func3

  initial $display("%b", p1);

endmodule // constfunc2
