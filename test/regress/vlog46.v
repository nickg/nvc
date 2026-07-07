module vlog46;
  integer i;
  integer sum = 0;

  initial begin
    begin : for_loop
      for (i = 0; i < 10; i = i + 1) begin
        if (i == 4)
          disable for_loop;
        sum = sum + 1;
      end
    end

    i = 0;
    begin : while_loop
      while (i < 10) begin
        i = i + 1;
        if (i == 3)
          disable while_loop;
        sum = sum + 10;
      end
    end

    begin : repeat_loop
      repeat (10) begin
        sum = sum + 100;
        disable repeat_loop;
      end
    end

    begin : no_loop
      sum = sum + 1;
      disable no_loop;
      sum = sum + 100;
    end

    if (sum == 125)
      $display("PASSED");
    else
      $display("FAILED: sum=%0d", sum);
  end
endmodule // vlog46
