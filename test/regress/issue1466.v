module issue1466_addr
  #(parameter AW = 4)
  (
    input [AW-1:0] i_last_addr,
    input [AW-1:0] increment,
    input [1:0] i_burst,
    input [1:0] i_size,
    output reg [AW-1:0] o_next_addr
  );

  localparam [1:0] FIXED = 2'b00;

  always @(*)
    begin
      o_next_addr = i_last_addr + increment;
      if (i_burst != FIXED)
	begin
	  case(i_size[1:0])
	    2'b00:  o_next_addr = o_next_addr;
	    2'b01:  o_next_addr[  0] = 0;
	    2'b10:  o_next_addr[(AW-1>1) ? 1 : (AW-1):0]= 0;
	    2'b11:  o_next_addr[(AW-1>2) ? 2 : (AW-1):0]= 0;
	  endcase
	end
    end

endmodule // issue1466_addr

module issue1466;

  localparam [1:0] FIXED = 2'b00;
  localparam [1:0] INCR  = 2'b01;

  reg [1:0] i_burst, i_size;
  reg fail;

  reg [0:0] i_last_addr1, increment1;
  wire [0:0] o_next_addr1;

  reg [2:0] i_last_addr3, increment3;
  wire [2:0] o_next_addr3;

  reg [3:0] i_last_addr4, increment4;
  wire [3:0] o_next_addr4;

  issue1466_addr #(1) dut1 (
    .i_last_addr(i_last_addr1),
    .increment(increment1),
    .i_burst(i_burst),
    .i_size(i_size),
    .o_next_addr(o_next_addr1)
  );

  issue1466_addr #(3) dut3 (
    .i_last_addr(i_last_addr3),
    .increment(increment3),
    .i_burst(i_burst),
    .i_size(i_size),
    .o_next_addr(o_next_addr3)
  );

  issue1466_addr #(4) dut4 (
    .i_last_addr(i_last_addr4),
    .increment(increment4),
    .i_burst(i_burst),
    .i_size(i_size),
    .o_next_addr(o_next_addr4)
  );

  initial begin
    fail = 0;

    i_last_addr1 = 1'b0;
    increment1 = 1'b1;

    i_last_addr3 = 3'b110;
    increment3 = 3'b001;

    i_last_addr4 = 4'b1110;
    increment4 = 4'b0001;

    i_burst = INCR;
    i_size = 2'b00;
    #1;
    if (o_next_addr1 !== 1'b1 || o_next_addr3 !== 3'b111
        || o_next_addr4 !== 4'b1111) begin
      $display("FAILED (1) -- %b %b %b",
               o_next_addr1, o_next_addr3, o_next_addr4);
      fail = 1;
    end

    i_size = 2'b01;
    #1;
    if (o_next_addr1 !== 1'b0 || o_next_addr3 !== 3'b110
        || o_next_addr4 !== 4'b1110) begin
      $display("FAILED (2) -- %b %b %b",
               o_next_addr1, o_next_addr3, o_next_addr4);
      fail = 1;
    end

    i_size = 2'b10;
    #1;
    if (o_next_addr1 !== 1'b0 || o_next_addr3 !== 3'b100
        || o_next_addr4 !== 4'b1100) begin
      $display("FAILED (3) -- %b %b %b",
               o_next_addr1, o_next_addr3, o_next_addr4);
      fail = 1;
    end

    i_size = 2'b11;
    #1;
    if (o_next_addr1 !== 1'b0 || o_next_addr3 !== 3'b000
        || o_next_addr4 !== 4'b1000) begin
      $display("FAILED (4) -- %b %b %b",
               o_next_addr1, o_next_addr3, o_next_addr4);
      fail = 1;
    end

    i_burst = FIXED;
    #1;
    if (o_next_addr1 !== 1'b1 || o_next_addr3 !== 3'b111
        || o_next_addr4 !== 4'b1111) begin
      $display("FAILED (5) -- %b %b %b",
               o_next_addr1, o_next_addr3, o_next_addr4);
      fail = 1;
    end

    if (fail)
      $display("FAILED");
    else
      $display("PASSED");

    $finish;
  end

endmodule // issue1466
