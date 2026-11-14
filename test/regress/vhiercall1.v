// Hierarchical task and function calls
module vhiercall1_helper #(parameter [7:0] STEP = 1);
  reg [7:0] count = 0;

  function [7:0] add_step(input [7:0] x);
    add_step = x + STEP;
  endfunction

  function [7:0] scaled(input [7:0] x, input [7:0] k = 2);
    scaled = x * k + count;
  endfunction

  task bump(input [7:0] by);
    count = count + by;
  endtask

  task get(output [7:0] value);
    value = count;
  endtask

  task later_bump(input [7:0] by);
    #2 count = count + by;
  endtask

  task clear;
    count = 0;
  endtask
endmodule

module vhiercall1_wrapper;
  vhiercall1_helper #(.STEP(10)) inner();
endmodule

module vhiercall1;
  vhiercall1_helper h();
  vhiercall1_helper #(.STEP(5)) h5();
  reg [7:0] r1, r2, r3, r4, r5;
  wire [7:0] w = h.add_step(8'd4);
  wire [7:0] lw = later.add_step(8'd1);

  initial begin
    r1 = h.add_step(8'd41);
    r2 = h5.add_step(8'd41);
    h.bump(3);
    h.bump(4);
    h5.bump(1);
    r3 = h.scaled(8'd3);
    h.get(r4);
    r5 = w2.inner.add_step(8'd1);
    h5.later_bump(2);
    #1;
    h.clear;
    if (r1 !== 42 || r2 !== 46 || r3 !== 13 || r4 !== 7 || r5 !== 11
        || w !== 5 || lw !== 2 || h.count !== 0 || h5.count !== 3)
      $display("FAILED %0d %0d %0d %0d %0d %0d %0d %0d %0d", r1, r2, r3, r4,
               r5, w, lw, h.count, h5.count);
    else
      $display("PASSED");
  end

  vhiercall1_wrapper w2();
  vhiercall1_helper later();
endmodule
