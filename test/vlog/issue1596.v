module counter
(
    input  wire clk,
    input  wire reset,
    output wire [31:0] count_out
);

    reg [31:0] count;

    count_out = count;
    //assign count_out = count;

    always @(posedge clk) begin
        if (reset)
            count <= 32'b0;
        else
            count <= count + 1'b1;
    end

endmodule
