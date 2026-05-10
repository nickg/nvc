program lower3;

   int sum;
   logic idx; // Use this to test scope;
   initial begin
      sum = 0;
      idx = 1'bx;
      for (int idx = 0 ; idx < 8 ; idx += 1) begin
	 sum += idx;
      end
   end // initial begin

endprogram
