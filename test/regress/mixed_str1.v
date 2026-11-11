module strmod #(parameter MODE = "slow", parameter integer N = 3)
   (output reg [7:0] o);
  initial begin
    if (MODE == "fast")
      o = 10 + N;
    else
      o = 20 + N;
  end
endmodule
