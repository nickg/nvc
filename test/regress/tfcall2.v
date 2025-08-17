module tfcall2;
  localparam	C_AXIL_DATA_WIDTH = 32;

  function [C_AXIL_DATA_WIDTH-1:0] apply_wstrb;
    input   [C_AXIL_DATA_WIDTH-1:0]  prior_data;
    input [C_AXIL_DATA_WIDTH-1:0]    new_data;
    input [C_AXIL_DATA_WIDTH/8-1:0]  wstrb;

    integer                          k;

    begin
      $display("%x %x %x", prior_data, new_data, wstrb);
      for(k=0; k<C_AXIL_DATA_WIDTH/8; k=k+1)
        begin
	  apply_wstrb[k*8 +: 8]
	    = wstrb[k] ? new_data[k*8 +: 8] : prior_data[k*8 +: 8];
        end
    end
  endfunction // apply_wstrb

  reg	[2*C_AXIL_DATA_WIDTH-1:0]	wide_address;
  wire	[C_AXIL_DATA_WIDTH-1:0]	wskd_data;
  reg [C_AXIL_DATA_WIDTH/8-1:0]	wskd_strb;
  wire [C_AXIL_DATA_WIDTH-1:0]          new_cmdaddrlo;
  reg [C_AXIL_DATA_WIDTH-1:0]          tmp1, tmp2;

  assign	new_cmdaddrlo= apply_wstrb(wide_address[C_AXIL_DATA_WIDTH-1:0],
			                   wskd_data, wskd_strb);

  assign wskd_data = 32'hfeedface;

  initial begin
    wide_address = 'hdeadbeef;
    wskd_strb = 4'b0000;
    #1;
    tmp1 = new_cmdaddrlo;
    wskd_strb = 4'b0110;
    #2;
    tmp2 = new_cmdaddrlo;
    wskd_strb = 4'b1100;
    #1;
    $display("%x %x %x", tmp1, tmp2, new_cmdaddrlo);
    if (tmp1 === 32'hdeadbeef && tmp2 === 32'hdeedfaef && new_cmdaddrlo === 32'hfeedbeef)
      $display("PASSED");
    else
      $display("FAILED");
    $finish;
  end

endmodule // vlog25
