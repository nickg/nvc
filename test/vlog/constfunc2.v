module constfunc2;
  localparam p0 = 2 + 0;
  localparam p1 = func1(2, 3, 2'b10);

  function [(p0+1+1-2)*p0-1:0] func1;
    input integer x, y;
    input [p0-1:0] pat;
    func1 = {(p0+1+1-2){pat}};
  endfunction // func1

  initial $display("%b", p1);

endmodule // constfunc2
