// From ivtest/ivltests/nested_func.v
module tfcall8();

function automatic real sum;

input real a;
input real b;

begin
  sum = a + b;
end

endfunction

real r1;
real r2;
real r3;

initial begin
  r1 = sum(sum(2, 3), sum(4, 5));
  r2 = sum(3, sum(4, sum(5, 6)));
  r3 = sum(sum(sum(4, 5), 6), 7);
  if (r1 == 14 && r2 == 18 && r3 == 22)
    $display("PASSED");
  else
    $display("FAILED");
end

endmodule
