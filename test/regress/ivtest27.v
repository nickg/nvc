// From ivtest/ivltests/br967.v
module ivtest27();

integer count = 0;

initial begin
  repeat (10.4) begin
    count = count + 1;
    $display(count);
  end
  if (count === 10)
    $display("PASSED");
  else
    $display("FAILED");
end

endmodule
