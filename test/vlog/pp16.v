//------------------------------------------------------------------------------
//  This comment should not be reported as the location of the error below.
//------------------------------------------------------------------------------

`begin_keywords "1800-2017"

`define MEM_TYPE_RAM_SP  (memory_type == 0)
`define MEM_TYPE_RAM_TDP (memory_type == 2)
`define MEM_PORTB_WRITE (`MEM_TYPE_RAM_SP || `MEM_TYPE_RAM_TDP)

module pp16;
  reg [31:0] memory_init_file;
  integer memory_type;

  initial begin
    if (`MEM_PORTB_WRITE) begin
      module;  // Error
    end
  end
endmodule
