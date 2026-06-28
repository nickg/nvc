module child(
  input wire clk,
  input wire s_valid,
  output wire s_ready,
  output wire out
);
  reg ready_reg = 1'b1, ready_next = 1'b1;
  reg out_reg = 1'b0, out_next = 1'b0;

  assign s_ready = ready_reg;
  assign out = out_reg;

  always @* begin
    ready_next = 1'b1;
    out_next = out_reg;

    if (s_ready && s_valid) begin
      out_next = 1'b1;
      ready_next = 1'b0;
    end
  end

  always @(posedge clk) begin
    ready_reg <= ready_next;
    out_reg <= out_next;
  end
endmodule

module parent(
  input wire clk,
  input wire response,
  output wire out
);
  reg state_reg = 1'b0, state_next = 1'b0;
  reg valid_reg = 1'b0, valid_next = 1'b0;
  wire ready;

  child child_inst (
    .clk(clk),
    .s_valid(valid_reg),
    .s_ready(ready),
    .out(out)
  );

  always @* begin
    state_next = state_reg;
    valid_next = valid_reg && !ready;

    if (!state_reg && response) begin
      valid_next = 1'b1;
      state_next = 1'b1;
    end
  end

  always @(posedge clk) begin
    state_reg <= state_next;
    valid_reg <= valid_next;
  end
endmodule

module vlog45;
  reg clk = 1'b0;
  reg response = 1'b0;
  wire out;

  parent parent_inst (
    .clk(clk),
    .response(response),
    .out(out)
  );

  always #1 clk = ~clk;

  initial begin
    repeat (2) @(posedge clk);
    response <= 1'b1;
    @(posedge clk);
    response <= 1'b0;
    repeat (2) @(posedge clk);
    #1;

    if (out)
      $display("PASSED");
    else
      $display("FAILED");

    $finish;
  end
endmodule
