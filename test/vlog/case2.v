// Semantic errors
module case2;
  reg x, y, z;
  reg [7:0] a, b, c;

  initial begin
    case (a)  // OK
      1, 2: b = 5;
      default: b = 6;
    endcase // case (a)
    case (a)
      default b = 6;
      default b = 7; // Error
    endcase // case (a)
  end

endmodule // case2
