// Parse errors
module tfcall1;
  function [7:0] sum;
    input [7:0] x, y;   // OK
    sum = x + y;   // OK
  endfunction // sum

  assign x1 = sum(1, 2);     // OK

  assign x4 = double(2);     // OK
  assign x5 = not_here(1);   // Error
  assign x7 = no_args();     // OK

  function [3:0] double(input [3:0] x);
    double = x * 2;
  endfunction // double

  function [3:0] no_args( );
    no_args = 1;
  endfunction // no_args

  reg x, y;
  initial begin
    task1(x, y);        // OK
    void'(no_args());   // OK
  end

  task task1(input x, output y);
    y = x * 2;    // OK
  endtask // task1

  initial main;    // OK

  task main; endtask

  initial return;   // Error

  task task3;
  endtask : bob  // Error

  function func2;
  endfunction : blah  // Error

  typedef logic [15:0] word;

  function word func3;  // OK
  endfunction

endmodule // tfcall1
