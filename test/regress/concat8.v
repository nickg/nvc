module concat8;

  function integer double;
    input integer x;
    begin
      double = x * 2;
    end
  endfunction

  function integer add_one;
    input integer x;
    begin
      add_one = x + 1;
    end
  endfunction

  parameter integer WIDTH = double(4);   // 8

  // Replication count uses binary expression on a parameter computed
  // by a user function call.  The expressions below must be folded to
  // a constant integer before lowering.

  reg [9:0]  data1;
  reg [15:0] data2;
  reg [15:0] data3;
  reg [7:0]  data4;

  initial begin
    data1 = {(WIDTH + 1){1'b1}};           // 9 ones
    data2 = {(WIDTH << 1){1'b1}};          // 16 ones
    data3 = {(WIDTH * 2){1'b1}};           // 16 ones
    data4 = {(add_one(WIDTH) - 1){1'b1}};  // 8 ones

    $display("WIDTH = %0d", WIDTH);
    $display("data1 = %b", data1);
    $display("data2 = %b", data2);
    $display("data3 = %b", data3);
    $display("data4 = %b", data4);

    if (data1 === 10'h1FF
        && data2 === 16'hFFFF
        && data3 === 16'hFFFF
        && data4 === 8'hFF)
      $display("PASSED");
    else
      $display("FAILED");

    $finish;
  end

endmodule // constfunc2
