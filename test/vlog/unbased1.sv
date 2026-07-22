module unbased1;
  logic [7:0] a;

  localparam [7:0]        q0 = '0;          // 8'h00
  localparam [7:0]        q1 = '1;          // 8'hff
  localparam [3:0]        q2 = '1;          // 4'hf
  localparam logic [3:0]  qx = 'x;          // 4'bxxxx
  localparam logic [3:0]  qz = 'z;          // 4'bzzzz
  localparam [7:0]        r0 = 8'h0f | '1;  // 8'hff
  localparam [7:0]        r1 = 8'hf0 & '1;  // 8'hf0
  localparam              u1 = '1;          // 32-bit fill
  localparam [71:0]       big1 = '1;        // wider than one word

  initial a = '1;

endmodule // unbased1
