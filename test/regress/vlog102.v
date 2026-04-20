// Test ? as don't-care digit in casex patterns (IEEE 1364-2005 s9.5.3)
// Covers: binary, octal, hex radixes; mixing ? with x and z;
//         narrow and wide operands; x in expression and items.

module vlog102;

  integer fail = 0;

  task check(input [80*8-1:0] label, input ok);
    if (!ok) begin
      $display("FAILED: %0s", label);
      fail = 1;
    end
  endtask

  // --- 1. Basic ? in binary casex items ---
  reg [3:0] sel;
  reg [1:0] r1;

  always @(*) begin
    casex (sel)
      4'b1???:  r1 = 2'd3;
      4'b01??:  r1 = 2'd2;
      4'b001?:  r1 = 2'd1;
      4'b0001:  r1 = 2'd0;
      default:  r1 = 2'bxx;
    endcase
  end

  // --- 2. ? in octal casex item ---
  reg [8:0] osel;
  reg [1:0] r2;

  always @(*) begin
    casex (osel)
      9'o7??:  r2 = 2'd3;
      9'o3??:  r2 = 2'd2;
      9'o1??:  r2 = 2'd1;
      default: r2 = 2'd0;
    endcase
  end

  // --- 3. ? in hex casex item ---
  reg [7:0] hsel;
  reg [1:0] r3;

  always @(*) begin
    casex (hsel)
      8'hf?:   r3 = 2'd3;
      8'ha?:   r3 = 2'd2;
      8'h5?:   r3 = 2'd1;
      default:  r3 = 2'd0;
    endcase
  end

  // --- 4. Mixing ? with x in the same literal ---
  reg [7:0] mix;
  reg [1:0] r4;

  always @(*) begin
    casex (mix)
      8'b1??x_x??0:  r4 = 2'd1;
      8'b0?x?_?x?1:  r4 = 2'd2;
      default:        r4 = 2'd0;
    endcase
  end

  // --- 5. Mixing ?, x, and z together ---
  reg [7:0] triple;
  reg [1:0] r5;

  always @(*) begin
    casex (triple)
      8'b1?xz_??x0:  r5 = 2'd1;
      8'b0z?x_xz?1:  r5 = 2'd2;
      default:        r5 = 2'd0;
    endcase
  end

  // --- 6. x/z in casex expression (both are don't-care in casex) ---
  reg [3:0] xsel;
  reg [1:0] r6;

  always @(*) begin
    casex (xsel)
      4'b??10:  r6 = 2'd1;
      4'b??01:  r6 = 2'd2;
      default:  r6 = 2'd0;
    endcase
  end

  // --- 7. Wide (>64 bit) casex with ? ---
  reg [79:0] wide;
  reg [1:0] r7;

  always @(*) begin
    casex (wide)
      {16'hf??f, 64'h0000_0000_0000_0001}: r7 = 2'd1;
      {16'h?00?, 64'hffff_ffff_ffff_ffff}: r7 = 2'd2;
      default:                              r7 = 2'd0;
    endcase
  end

  // --- 8. All-? matches everything (casex) ---
  reg [3:0] anysel;
  reg [1:0] r8;

  always @(*) begin
    casex (anysel)
      4'b????:  r8 = 2'd1;
      default:  r8 = 2'd0;
    endcase
  end

  initial begin
    // Test 1: binary ? items in casex
    sel = 4'b1010; #1;
    check("casex bin ? MSB=1", r1 === 2'd3);
    sel = 4'b0110; #1;
    check("casex bin ? top=01", r1 === 2'd2);
    sel = 4'b0010; #1;
    check("casex bin ? top=001", r1 === 2'd1);
    sel = 4'b0001; #1;
    check("casex bin ? exact", r1 === 2'd0);

    // Test 2: octal ? items in casex
    osel = 9'o710; #1;
    check("casex oct ? top=7", r2 === 2'd3);
    osel = 9'o345; #1;
    check("casex oct ? top=3", r2 === 2'd2);
    osel = 9'o177; #1;
    check("casex oct ? top=1", r2 === 2'd1);
    osel = 9'o077; #1;
    check("casex oct ? default", r2 === 2'd0);

    // Test 3: hex ? items in casex
    hsel = 8'hf7; #1;
    check("casex hex ? top=f", r3 === 2'd3);
    hsel = 8'ha3; #1;
    check("casex hex ? top=a", r3 === 2'd2);
    hsel = 8'h5c; #1;
    check("casex hex ? top=5", r3 === 2'd1);
    hsel = 8'h20; #1;
    check("casex hex ? default", r3 === 2'd0);

    // Test 4: mixed ? and x
    mix = 8'b1110_0100; #1;
    check("casex mix ?/x pattern1", r4 === 2'd1);
    mix = 8'b0100_0011; #1;
    check("casex mix ?/x pattern2", r4 === 2'd2);
    mix = 8'b0000_0100; #1;
    check("casex mix ?/x default", r4 === 2'd0);

    // Test 5: mixing ?, x, and z
    triple = 8'b1010_0100; #1;
    check("casex triple ?/x/z pat1", r5 === 2'd1);
    triple = 8'b0100_0011; #1;
    check("casex triple ?/x/z pat2", r5 === 2'd2);
    triple = 8'b0000_0100; #1;
    check("casex triple ?/x/z default", r5 === 2'd0);

    // Test 6: x and z in casex expression are don't-care
    xsel = 4'bxx10; #1;
    check("casex x in expr pat1", r6 === 2'd1);
    xsel = 4'bzz01; #1;
    check("casex z in expr pat2", r6 === 2'd2);
    xsel = 4'b?z11; #1;
    check("casex z in expr default", r6 === 2'd0);

    // Test 7: wide casex with ?
    wide = {16'hf00f, 64'h0000_0000_0000_0001}; #1;
    check("casex wide ? match1", r7 === 2'd1);
    wide = {16'h1002, 64'hffff_ffff_ffff_ffff}; #1;
    check("casex wide ? match2", r7 === 2'd2);
    wide = {16'h1234, 64'h0000_0000_0000_0000}; #1;
    check("casex wide ? default", r7 === 2'd0);

    // Test 8: all-? matches any value including x/z
    anysel = 4'b1010; #1;
    check("casex all-? matches", r8 === 2'd1);
    anysel = 4'bxz01; #1;
    check("casex all-? matches xz", r8 === 2'd1);

    if (fail == 0)
      $display("PASSED");
    $finish;
  end

endmodule
