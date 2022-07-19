package p1 is
    constant c1 : integer := 1;
end package;

package p2 is
    constant c2 : integer := 2;
end package;

package p3 is
    constant c3 : integer := 3;
end package;

package p4 is
    constant c4 : integer := 4;
end package;

package p5 is
    constant c5 : integer := 5;
end package;

package p6 is
    constant c6 : integer := 6;
end package;

entity issue225 is
    use work.p1.all;
end entity issue225;
architecture test of issue225 is
    use work.p2.all;                      -- doesn't work
begin
    g1: if true generate
        use work.p3.all;                  -- doesn't work
    begin
        b1: block
            use work.p4.all;              -- doesn't work
        begin
            pp1: process
                use work.p5.all;          -- doesn't work
                procedure doit is
                    use work.p6.all;      -- doesn't work
                    variable x : integer;
                begin
                    x := c1 + c2 + c3 + c4 + c5 + c6;
                    wait;
                end procedure doit;
            begin
                doit;
            end process pp1;
        end block b1;
    end generate g1;
end architecture test;
