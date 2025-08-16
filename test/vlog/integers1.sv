module integers1;
  logic [9:0]  a;
  logic [19:0] b;
  logic [12:0] c;
  logic [10:0] d;
  logic [3:0]  e;
  logic [4:0]  f;
  logic [2:0]  g;
  logic [11:0] h;
  logic [15:0] i;
  logic [7:0]  l;
  logic [7:0]  m;
  logic [3:0]  n;
  logic [3:0]  o;
  logic [15:0] p;

  initial begin
    a = 659;      // is a decimal number
    b = 'h 837FF; // is a hexadecimal number
    c = 'o7460;   // is an octal number
    d = 4af;      // is illegal (hexadecimal format requires 'h)
    e = 4'b1001;  // is a 4-bit binary number
    f = 5 'D 3;   // is a 5-bit decimal number
    g = 3'b01x;   // is a 3-bit number with the least
                  // significant bit unknown
    h = 12'hx;    // is a 12-bit unknown number
    i = 16'hz;    // is a 16-bit high-impedance number
    l = 8 'd -6;  // this is illegal syntax
    m = -8 'd 6;  // this defines the two's-complement of 6,
                  // held in 8 bits—equivalent to -(8'd 6)
    n = 4 'shf;   // this denotes the 4-bit number '1111', to
                  // be interpreted as a two's-complement number,
                  // or '-1'. This is equivalent to -4'h 1
    o = -4 'sd15; // this is equivalent to -(-4'd 1), or '0001'
    p = 16'sd?;   // the same as 16'sbz

  end

endmodule // integers1
