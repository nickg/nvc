module array22;
   reg clk = 0;
   reg wr = 0;
   reg [1:0] waddr = 0;
   reg [1:0] raddr = 2;
   reg [31:0] wdata = 0;
   reg [31:0] mem [0:3];
   reg [31:0] rdata;

   always #5 clk = ~clk;

   always @(posedge clk) begin
      if (wr)
         mem[waddr] <= wdata;
   end

   always @* begin
      rdata = raddr ? mem[raddr] : 0;
   end

   initial begin
      #1;
      waddr = 2;
      wdata = 32'h12345678;
      wr = 1;
      @(posedge clk);
      #1;
      wr = 0;
      #1;
      if (rdata !== 32'h12345678)
         $display("FAILED");
      else
         $display("PASSED");
      $finish;
   end
endmodule
