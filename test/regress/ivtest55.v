`define msg(x,y) `"x: `\`"y`\`"`"

// From ivltests/sv_macro2.v
module ivtest55();

initial begin
  $display(`msg(left side,right side));
end

endmodule
