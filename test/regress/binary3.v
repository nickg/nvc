module binary3();

  reg [31:0] a, b;
  reg [65:0] a_w, b_w;

  wire [31:0] div = a / b;
  wire [31:0] mod = a % b;
  wire [65:0] div_w = a_w / b_w;
  wire [65:0] mod_w = a_w % b_w;

  reg         failed = 0;
  integer     i;

  initial begin
    a = 'h1;
    b = 'h1;
    a_w = 'h1;
    b_w = 'h1;
    #1;
    if (div !== 1 || mod !== 0 || div_w !== 1 || mod_w !== 0)
      failed = 1;

    a = 'h2537f12;
    b = 'h7322a;
    a_w = 'h2537f12;
    b_w = 'h7322a;
    #1;
    if (div !== 'h52 || mod !== 'h56d9e || div_w !== 'h52 || mod_w !== 'h56d9e)
      failed = 1;

    b = 0;
    b_w = 0;
    #1;
    if (div !== 'hx || mod !== 'hx /*|| div_w !== 'hx || mod_w !== 'hx*/)
      failed = 1;

    if (failed) begin
      $display("FAILED");
      $finish;
    end

    for (i = 0; i < 100; i = i + 1) begin
      a = $random;
      b = $random;
      a_w = a;
      b_w = b;
      #1;
      if (div !== div_w || mod !== mod_w) begin
        $display("FAILED");
        $finish;
      end
    end

    $display("PASSED");
  end

  always @*
    $display("%x / %x = %x; %x %% %x = %x",
             a, b, div, a, b, mod);

  always @*
    $display("%x / %x = %x; %x %% %x = %x",
             a_w, b_w, div_w, a_w, b_w, mod_w);

endmodule // binary3
