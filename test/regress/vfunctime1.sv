module vfunctime1;
  function time scaled;
    input integer n;
    const integer k = 1000;
    begin
      scaled = n * k;
    end
  endfunction

  function longint wide(input int a);
    const longint big = 64'h1_0000_0000;
    return big + a;
  endfunction

  function byte low(input int a);
    return a[7:0];
  endfunction

  initial begin
    if (scaled(3) !== 3000) $display("FAILED scaled");
    else if (wide(5) !== 64'h1_0000_0005) $display("FAILED wide");
    else if (low(32'h1234) !== 8'h34) $display("FAILED low");
    else $display("PASSED");
  end
endmodule
