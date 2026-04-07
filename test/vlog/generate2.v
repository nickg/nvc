// Test for generate case and else-if constructs
module generate2;
  parameter MODE = 1;
  parameter SEL = 2;

  // Generate case
  generate
    case (MODE)
      0: begin : gen_mode0
        localparam P = 10;
      end
      1: begin : gen_mode1
        localparam P = 20;
      end
      2: begin : gen_mode2
        localparam P = 30;
      end
    endcase
  endgenerate

  // Generate else-if
  generate
    if (SEL == 0) begin : gen_sel0
      localparam Q = 100;
    end else if (SEL == 1) begin : gen_sel1
      localparam Q = 200;
    end else if (SEL == 2) begin : gen_sel2
      localparam Q = 300;
    end else begin : gen_sel_default
      localparam Q = 400;
    end
  endgenerate
endmodule
