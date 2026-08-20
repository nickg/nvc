module issue1640;
  parameter FILE_NAME = "test.txt";

  initial begin
    if (32'(FILE_NAME) == ".txt")
      $display("PASSED");
    else
      $display("FAILED");
  end
endmodule // issue1640
