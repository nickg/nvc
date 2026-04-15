// Port connection and system task args with hierarchical references.
//
// Port connection: sub v (.p(u.x)) where u.x is a hier ref as actual.
// System tasks: $display("%b", u.x), $monitor(u.x), $fwrite(fd, ..., u.x).

module vlog70_src;
    reg [7:0] x = 8'hA5;
endmodule

module vlog70_sink(input [7:0] p);
    reg [7:0] captured;
    initial begin
        #1;
        captured = p;
    end
endmodule

module vlog70;
    vlog70_src u ();

    // Port connection using hier ref as actual expression.
    vlog70_sink v (.p(u.x));

    integer fd;

    initial begin
        #2;

        // Check port connection propagated correctly.
        if (v.captured !== 8'hA5) begin
            $display("FAILED: port connection v.captured=%h expected A5",
                     v.captured);
            $finish;
        end

        // $display with hier ref arg.
        $display("display: u.x=%b", u.x);

        // $monitor with a single hier ref value for grep-friendly output.
        $monitor("monitor: u.x=%h", u.x);

        // $fwrite to a file with hier ref arg.
        fd = $fopen("vlog70.out", "w");
        if (fd == 0) begin
            $display("FAILED: could not open vlog70.out");
            $finish;
        end
        $fwrite(fd, "fwrite: u.x=%h\n", u.x);
        $fclose(fd);

        // Change value so $monitor fires once more.
        u.x = 8'hBB;
        #1;

        $display("PASSED");
        $finish;
    end
endmodule
