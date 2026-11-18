// Constant selects of unpacked array local parameters
module vlparray1_child #(parameter [1:0] P = 0, parameter S = "",
                         parameter int N = 0) (output [1:0] p,
                                               output [8*4:1] s,
                                               output [31:0] n);
  assign p = P;
  assign s = S;
  assign n = N;
endmodule

module vlparray1;
  localparam [1:0] a0 = 2'b01, a1 = 2'b10;
  localparam bit [1:0] arr [1:0] = {a1, a0};
  localparam int nums [3:0] = {3, 2, 1, 0};
  localparam string names [0:1] = {"zero", "one"};
  localparam int up [0:2] = {10, 20, 30};

  wire [3:0]  pp;
  wire [63:0] ss, nn;

  genvar i;
  for (i = 0; i < 2; i = i + 1) begin : g
    vlparray1_child #(.P(arr[i]), .S(names[i]), .N(nums[i + 2]))
      c(pp[2*i +: 2], ss[32*i +: 32], nn[32*i +: 32]);
  end

  localparam int second = up[1];

  initial begin
    #1;
    if (pp !== 4'b1001)
      $display("FAILED p %b", pp);
    if (ss[31:0] != "zero" || ss[63:32] != "one")
      $display("FAILED s %s %s", ss[31:0], ss[63:32]);
    if (nn !== {32'd3, 32'd2})
      $display("FAILED n %h", nn);
    if (second !== 20)
      $display("FAILED second %0d", second);
    $display("PASSED");
  end
endmodule
