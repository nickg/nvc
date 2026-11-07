module sub10 #(parameter MODE = "auto", parameter real SCALE = 1.0,
             parameter WIDTH = 4, parameter NAME = "xyz")
   (output reg [31:0] o);
   initial begin
      o = (MODE == "fast" ? 1000 : 2000) + $rtoi(SCALE * 100) + WIDTH
        + (NAME == "def" ? 3 : 0);
   end
endmodule
