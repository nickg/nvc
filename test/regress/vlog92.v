// Case-generate prefix: hierarchical access into the branch of a
// generate-case block.  Fills a gap left by vlog61 which covers only
// for-generate and if-generate.  IEEE 1364-2005 §12.1.3.

module vlog92_sub;
    reg [3:0] x = 4'hA;
endmodule

module vlog92;
    parameter MODE = 1;

    generate
        case (MODE)
            0: begin : gen_m0
                vlog92_sub u ();
                reg [3:0] marker = 4'h0;
            end
            1: begin : gen_m1
                vlog92_sub u ();
                reg [3:0] marker = 4'h1;
            end
            default: begin : gen_md
                vlog92_sub u ();
                reg [3:0] marker = 4'hF;
            end
        endcase
    endgenerate

    initial begin
        #1;
        if (gen_m1.u.x !== 4'hA) begin
            $display("FAILED: gen_m1.u.x=%h expected A", gen_m1.u.x);
            $finish;
        end
        if (gen_m1.marker !== 4'h1) begin
            $display("FAILED: gen_m1.marker=%h expected 1", gen_m1.marker);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule
