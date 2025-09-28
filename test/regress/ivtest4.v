module ivtest4;

   reg  cp;
   reg	d;
   wire q;

   dff ff(q, cp, d);

   always begin #5 cp=0; #5 cp=1; end

   always
     begin
	@(negedge cp)
	  d <= ~d;

	@(posedge cp) begin
	  #0;
	  if ((q === 1 || q === 0)  && d !== q)
	    begin
	       $display("FAILED, d=%b, q=%b", d, q);
	       #1 $finish;
	    end
       end
     end

   initial
     begin
	#1 d <= 1;
	#22;
	$display("PASSED");
	$finish;
     end

   initial $monitor($time,,cp,,d,,q);

endmodule

primitive dff(q, cp, d);
   output q;
   input  cp, d;
   reg	  q;

   table
   // (cp)  d  :  q  :  q  ;
        ?   *  :  ?  :  -  ;
      (?0)  ?  :  ?  :  -  ;
      (1x)  ?  :  ?  :  -  ;
      (x1)  0  :  0  :  0  ;
      (x1)  1  :  1  :  1  ;
      (0x)  0  :  0  :  0  ;
      (0x)  1  :  1  :  1  ;
      (01)  0  :  ?  :  0  ;
      (01)  1  :  ?  :  1  ;
   endtable

endprimitive
