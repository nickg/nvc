module file16;
  integer fd;
  integer n;
  integer i;
  integer failed;

  reg [31:0] dec;
  reg [7:0]  hex;
  reg [7:0]  bin;
  reg [7:0]  oct;
  reg [63:0] str;
  reg [7:0]  ch;
  reg [63:0] line;
  reg [9599:0] bigline;
  reg [7:0]  mem [0:1];

  initial begin
    failed = 0;

    fd = $fopen("file16.tmp", "w");
    if (fd == 0)
      failed = 1;

    $fdisplay(fd, "123 ab 1010 17 hi Z");
    $fwriteh(fd, 8'h5a);
    $fwrite(fd, "\nline\n");
    for (i = 0; i < 110; i = i + 1)
      $fwrite(fd, "0123456789");
    $fwrite(fd, "\n");
    $fflush(fd);
    $fclose(fd);

    fd = $fopen("file16.tmp", "r");
    n = $fscanf(fd, "%d %h %b %o %s %c\n", dec, hex, bin, oct, str, ch);
    if (n != 6 || dec != 123 || hex != 8'hab || bin != 8'd10
        || oct != 8'd15 || str[15:0] != "hi" || ch != "Z")
      failed = 1;

    n = $fgets(line, fd);
    if (n != 3 || line[23:0] != "5a\n")
      failed = 1;

    n = $fgets(line, fd);
    if (n != 5 || line[39:0] != "line\n")
      failed = 1;

    n = $fgets(bigline, fd);
    if (n != 1101)
      failed = 1;

    n = $fgets(line, fd);
    if (n != 0 || !$feof(fd))
      failed = 1;

    $fclose(fd);

    n = $sscanf("42 ff 11 ok Q", "%d %h %b %s %c",
                dec, hex, bin, str, ch);
    if (n != 5 || dec != 42 || hex != 8'hff || bin != 8'd3
        || str[15:0] != "ok" || ch != "Q")
      failed = 1;

    fd = $fopen("file16.mem", "w");
    $fdisplay(fd, "1010");
    $fdisplay(fd, "0101");
    $fclose(fd);

    $readmemb("file16.mem", mem);
    if (mem[0] != 8'd10 || mem[1] != 8'd5)
      failed = 1;

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule
