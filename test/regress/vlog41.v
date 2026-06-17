module vlog41;
  initial begin
    $info("info %d", 123);
    $warning("warning %s", "hello");
    $error("error %f", 1.5);
    $fatal(1, "fatal");
  end
endmodule // vlog41
