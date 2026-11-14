// Output and inout arguments of a task that waits
module vtaskout1;
  reg [7:0] r1, r2, acc = 8'd1, acc2 = 8'd7;
  integer   errors = 0;

  task automatic delayed(input [7:0] x, output [7:0] y, inout [7:0] z);
    begin
      #5 y = x;
      #1 z = z + 1;
    end
  endtask

  task twice(output [7:0] y);
    reg [7:0] tmp;
    begin
      #2 tmp = 8'h10;
      #2 y = tmp + 1;
    end
  endtask

  initial begin
    delayed(8'ha5, r1, acc);
    if ($time != 6 || r1 !== 8'ha5 || acc !== 8'd2) errors = errors + 1;
    twice(r1);
    if ($time != 10 || r1 !== 8'h11) errors = errors + 1;
  end

  initial begin
    #1 delayed(8'h3c, r2, acc2);
    if ($time != 7 || r2 !== 8'h3c || acc2 !== 8'd8) errors = errors + 1;
  end

  initial begin
    #20;
    if (errors == 0) $display("PASSED");
    else $display("FAILED errors=%0d", errors);
  end
endmodule
