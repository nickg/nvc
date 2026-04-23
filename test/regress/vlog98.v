// SystemVerilog struct member access via hierarchical reference.
// IEEE 1800 §7.2: struct member composed on a hier-ref base.
// Fills a gap left by vlog79 which substituted packed arrays for
// structs.  Uses anonymous struct declaration (the form nvc's parser
// unit tests exercise in struct1.sv), reached through a hier prefix.

module vlog98_sub;
    struct packed {
        logic [3:0] hdr;
        logic [3:0] payload;
    } pkt;

    initial begin
        pkt.hdr     = 4'hA;
        pkt.payload = 4'h5;
    end
endmodule

module vlog98;
    vlog98_sub u ();
    logic [3:0] h, p;

    initial begin
        #1;
        h = u.pkt.hdr;
        p = u.pkt.payload;
        if (h !== 4'hA || p !== 4'h5) begin
            $display("FAILED: h=%h p=%h expected A 5", h, p);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule
