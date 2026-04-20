// Test ? as don't-care digit in casez patterns (IEEE 1364-2005 s9.5.2)
// Covers: binary, octal, hex radixes; ? in case expression vs item;
//         mixing ? with z and x; narrow and wide operands; default fallthrough.

module vlog101;

  integer fail = 0;

  task check(input [80*8-1:0] label, input ok);
    if (!ok) begin
      $display("FAILED: %0s", label);
      fail = 1;
    end
  endtask

  // --- 1. Basic ? in binary case items ---
  reg [3:0] sel;
  reg [1:0] r1;

  always @(*) begin
    casez (sel)
      4'b1???:  r1 = 2'd3;
      4'b01??:  r1 = 2'd2;
      4'b001?:  r1 = 2'd1;
      4'b0001:  r1 = 2'd0;
      default:  r1 = 2'bxx;
    endcase
  end

  // --- 2. ? in octal case item ---
  reg [8:0] osel;
  reg [1:0] r2;

  always @(*) begin
    casez (osel)
      9'o7??:  r2 = 2'd3;
      9'o3??:  r2 = 2'd2;
      9'o1??:  r2 = 2'd1;
      default: r2 = 2'd0;
    endcase
  end

  // --- 3. ? in hex case item ---
  reg [7:0] hsel;
  reg [1:0] r3;

  always @(*) begin
    casez (hsel)
      8'hf?:   r3 = 2'd3;
      8'ha?:   r3 = 2'd2;
      8'h5?:   r3 = 2'd1;
      default:  r3 = 2'd0;
    endcase
  end

  // --- 4. ? in case expression (not just items) ---
  reg [3:0] data;
  reg [1:0] r4;

  always @(*) begin
    casez (data)
      4'b?001:  r4 = 2'd1;
      4'b?010:  r4 = 2'd2;
      4'b?100:  r4 = 2'd3;
      default:  r4 = 2'd0;
    endcase
  end

  // --- 5. Mixing ? and z in the same literal ---
  reg [7:0] mix;
  reg [1:0] r5;

  always @(*) begin
    casez (mix)
      8'b1??z_zz??:  r5 = 2'd1;
      8'b0?z?_?z?0:  r5 = 2'd2;
      default:        r5 = 2'd0;
    endcase
  end

  // --- 6. ? with expression containing z in case selector ---
  reg [3:0] zsel;
  reg [1:0] r6;

  always @(*) begin
    casez (zsel)
      4'b1???:  r6 = 2'd1;
      4'b01??:  r6 = 2'd2;
      default:  r6 = 2'd0;
    endcase
  end

  // --- 7. Wide (>64 bit) casez with ? ---
  reg [79:0] wide;
  reg [1:0] r7;

  always @(*) begin
    casez (wide)
      {16'hff??, 64'h0000_0000_0000_0001}: r7 = 2'd1;
      {16'h00??, 64'hffff_ffff_ffff_ffff}: r7 = 2'd2;
      default:                              r7 = 2'd0;
    endcase
  end

  // --- 8. All-? pattern (matches everything before default) ---
  reg [3:0] anysel;
  reg [1:0] r8;

  always @(*) begin
    casez (anysel)
      4'b????:  r8 = 2'd1;
      default:  r8 = 2'd0;
    endcase
  end

  initial begin
    // Test 1: binary ? items
    sel = 4'b1010; #1;
    check("bin ? MSB=1", r1 === 2'd3);
    sel = 4'b0110; #1;
    check("bin ? top=01", r1 === 2'd2);
    sel = 4'b0010; #1;
    check("bin ? top=001", r1 === 2'd1);
    sel = 4'b0001; #1;
    check("bin ? exact", r1 === 2'd0);

    // Test 2: octal ? items
    osel = 9'o710; #1;
    check("oct ? top=7", r2 === 2'd3);
    osel = 9'o345; #1;
    check("oct ? top=3", r2 === 2'd2);
    osel = 9'o177; #1;
    check("oct ? top=1", r2 === 2'd1);
    osel = 9'o077; #1;
    check("oct ? default", r2 === 2'd0);

    // Test 3: hex ? items
    hsel = 8'hf7; #1;
    check("hex ? top=f", r3 === 2'd3);
    hsel = 8'ha3; #1;
    check("hex ? top=a", r3 === 2'd2);
    hsel = 8'h5c; #1;
    check("hex ? top=5", r3 === 2'd1);
    hsel = 8'h20; #1;
    check("hex ? default", r3 === 2'd0);

    // Test 4: ? in items, specific low bits
    data = 4'b1001; #1;
    check("item ? bit0=001", r4 === 2'd1);
    data = 4'b0010; #1;
    check("item ? bit0=010", r4 === 2'd2);
    data = 4'b1100; #1;
    check("item ? bit0=100", r4 === 2'd3);
    data = 4'b0000; #1;
    check("item ? default", r4 === 2'd0);

    // Test 5: mixed ? and z
    mix = 8'b1110_0100; #1;
    check("mix ?/z pattern1", r5 === 2'd1);
    mix = 8'b0100_0010; #1;
    check("mix ?/z pattern2", r5 === 2'd2);
    mix = 8'b0000_0001; #1;
    check("mix ?/z default", r5 === 2'd0);

    // Test 6: z bits in case expression (z in expr acts as don't-care in casez)
    zsel = 4'bz100; #1;
    check("z in expr matches 1???", r6 === 2'd1);
    zsel = 4'b0100; #1;
    check("plain matches 01??", r6 === 2'd2);

    // Test 7: wide casez with ?
    wide = {16'hff00, 64'h0000_0000_0000_0001}; #1;
    check("wide ? match1", r7 === 2'd1);
    wide = {16'h0033, 64'hffff_ffff_ffff_ffff}; #1;
    check("wide ? match2", r7 === 2'd2);
    wide = {16'h1234, 64'h0000_0000_0000_0000}; #1;
    check("wide ? default", r7 === 2'd0);

    // Test 8: all-? matches any value
    anysel = 4'b1010; #1;
    check("all-? matches", r8 === 2'd1);
    anysel = 4'bxz01; #1;
    check("all-? matches xz", r8 === 2'd1);

    if (fail == 0)
      $display("PASSED");
    $finish;
  end

endmodule
