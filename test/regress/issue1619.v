module issue1619;

  reg clk = 0;
  always #5 clk = ~clk;

  // Onehot value that rotates 001, 010, 100, 001 ...
  reg [2:0] rotor = 3'b001;

  // Shared continuous assign net. Somehow when removing this and waiting on
  // 'rotor' directly hides the bug.
  wire [2:0] shared_net = rotor;

  reg extra = 0;

  // Victim, output stays 'x'.
  reg buggy_out;
  always @(shared_net)
    case (shared_net)
      3'b100:  buggy_out = 1;
      3'b010:  buggy_out = 0;
      3'b001:  buggy_out = 0;
      default: buggy_out = 1;
    endcase

  // Culprit? After removing this block the discrepancy dissapears.
  reg dummy_out;
  always @(shared_net, extra)
    case (shared_net)
      3'b100:  dummy_out = 1;
      3'b010:  dummy_out = 0;
      3'b001:  dummy_out = 0;
      default: dummy_out = 1;
    endcase

  // Logic-wise identical but continuous assignment.
  wire reference_out = (shared_net == 3'b100) ? 1 :
                       (shared_net == 3'b010) ? 0 :
                       (shared_net == 3'b001) ? 0 : 1;

  reg  fail = 0;
  integer n = 0;
  initial forever @(posedge clk) begin
    rotor = {rotor[1:0], rotor[2]}; // rotate onehot
    extra = ~extra;
    #1;
    if (buggy_out !== reference_out) begin
      $display("FAILED -- t=%0t shared_net=%b buggy_out=%b reference_out=%b",
               $time, shared_net, buggy_out, reference_out);
      fail = 1;
    end
    n = n + 1;
    if (n == 6) begin
      if (!fail) $display("PASSED");
      $finish;
    end
  end

endmodule // issue1619
