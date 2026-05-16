// Semantic errors
module tfcall2;
  function [7:0] sum;
    input [7:0] x, y;   // OK
    sum = x + y;   // OK
  endfunction // sum

  assign x1 = sum(1, 2);     // OK
  assign x2 = sum(1);        // Error
  assign x3 = sum(1, 2, 3);  // Error

  assign x4 = double(2);     // OK
  assign x6 = x4(6);         // Error
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
    no_args();          // Error
    void'(no_args());   // OK
  end

  task task1(input x, output y);
    y = x * 2;    // OK
  endtask // task1

  initial main;    // OK

  task main; endtask

  task task2;
    return 5;    // Error
  endtask : task2

  function logic func1;
    return;  // Error
  endfunction : func1

  task task3;
    input x;
    real  x;  // OK
    logic y;
    output y;  // OK
    input logic [3:0] z;
    logic [3:0]       z;  // Error
  endtask // task4

  function optarg1(integer x, integer y = 2);
  endfunction // optarg1

  task task4;
    integer r1 = optarg1();   // Error
    integer r2 = optarg1(5, 5, 5);   // Error
    r1 = optarg1(6);     // OK
    r1 = optarg1(6, 7);  // OK
    r1 = optarg1(, 7);   // Error
  endtask // task4

  function optarg2(integer x = 5, integer y);  // OK
  endfunction // optarg2

  task task5;
    integer r = optarg2();   // Error
    r = optarg2(1);   // Error
    r = optarg2(, 3); // OK
  endtask // task5

endmodule // tfcall1
