// From ivtest/ivltests/pr2138979c.v

module signed2();

reg   [7:0] a;
wire [15:0] y;
wire [15:0] z;

initial begin
    // Example vector
    a = 8'b10110110;

    // Wait for results to be calculated
    #1;

    // Display results
    $display("a = %b", a);
    $display("y = %b", y);
    $display("z = %b", z);

    if (a !== 8'b10110110
        || y !== 16'b1111111110110110
        || z !== 16'b1111111110110110)
      $display("FAILED");
    else
      $display("PASSED");

    // Finished
    $finish(0);
end

// Calculate signed logical OR
manually_extended_assignment INST1(.a(a), .y(y));
signed_assignment            INST2(.a(a), .y(z));

endmodule

module manually_extended_assignment(a, y);

input   [7:0] a;
output [15:0] y;

// Manually sign extend before assignment
assign y = {{8{a[7]}}, a};

endmodule

module signed_assignment(a, y);

input   [7:0] a;
output [15:0] y;

// $signed() sign extends before assignment
assign y = $signed(a);

endmodule
