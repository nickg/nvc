module issue1678(input a, output z);
  specify
    (a => z) = (100:100:100, 100:100:100);
    specparam PATHPULSE$ = 0;
  endspecify
endmodule
