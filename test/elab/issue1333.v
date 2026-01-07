`timescale 1ns/10ps
`celldefine
module sg13g2_inv_2 (Y, A);
	output Y;
	input A;

	// Function
	not (Y, A);

	// Timing
	specify
		(A => Y) = 0;
	endspecify
endmodule
`endcelldefine


module issue1333;
  wire clknet_leaf_15_clk;
  sg13g2_inv_2 clkload23 (.A(clknet_leaf_15_clk));
endmodule // issue1333
