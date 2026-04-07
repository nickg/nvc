// Test for constant user functions in if-generate conditions
// and block-local variables in function bodies.
module vlog37;

  // Function with begin:name block and local variables
  function integer double;
    input integer x;
    begin : body
      integer tmp;
      tmp = x * 2;
      double = tmp;
    end
  endfunction

  // Constant function call in if-generate condition
  wire dcheck;
  generate
    if (double(3) == 6) begin : gen_ok
      assign dcheck = 1;
    end else begin : gen_fail
      assign dcheck = 0;
    end
  endgenerate

  initial begin
    #1;
    $display("DCHECK=%0d", dcheck);
    if (dcheck !== 1'b1) $display("FAIL");
    $finish;
  end
endmodule
