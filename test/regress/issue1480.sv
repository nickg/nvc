module issue1480;
  logic [7:0]  limit = 0;
  logic [15:0] count = 0;

  reg [7:0]    mem [0:7];
  reg [7:0]    sum;

  initial begin
    #0;
    mem[0] = 8'd10;
    mem[1] = 8'd20;
    mem[2] = 8'd30;
    mem[3] = 8'd40;
    mem[4] = 8'd50;
    mem[5] = 8'd60;
    mem[6] = 8'd70;
    mem[7] = 8'd80;
  end

  always @(*) begin : sum_loop
    integer i;
    sum = 8'd0;
    for (i = 0; i < 8; i = i + 1) begin
      sum = sum + mem[i];
    end
  end

  always @(*) begin
    integer i;
    for (i = 0; i < limit; i++)
      count += i;
  end

  initial begin
    limit = 5;
    #1;
    $display("limit=%d count=%d sum=%d", limit, count, sum);
    limit = 6;
    #1;
    $display("limit=%d count=%d sum=%d", limit, count, sum);
    if (count === 25 && sum === 104)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // issue1480
