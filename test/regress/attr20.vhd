--
-- Example from LRM 19 page 282
--

package P is                                   -- P'PATH_NAME = ":lib:p:"
    procedure Proc (F: inout INTEGER);         -- P'INSTANCE_NAME = ":lib:p:"
    constant C: INTEGER := 42;                 -- Proc'PATH_NAME = ":lib:p:proc[integer]:"
    type IncPt1 is protected
        procedure increment;
        impure function get return INTEGER;
        procedure check (p, i, s : string);
    end protected IncPt1;
end package P;                                 -- Proc'INSTANCE_NAME = ":lib:p:proc[integer]:"

package body P is                              -- C'PATH_NAME = ":lib:p:c"
    procedure Proc (F: inout INTEGER) is       -- C'INSTANCE_NAME = ":lib:p:c"
        variable x: INTEGER;                   -- x'PATH_NAME = ":lib:p:proc[integer]:x"
    begin                                      -- x'INSTANCE_NAME = ":lib:p:proc[integer]:x"
        assert P'PATH_NAME = ":lib:p:";
        assert P'INSTANCE_NAME = ":lib:p:";
        assert Proc'PATH_NAME = ":lib:p:proc[integer]:";
        assert Proc'INSTANCE_NAME = ":lib:p:proc[integer]:";
        assert C'PATH_NAME = ":lib:p:c";
        assert C'INSTANCE_NAME = ":lib:p:c";
        assert x'PATH_NAME = ":lib:p:proc[integer]:x" report x'path_name;
        assert x'INSTANCE_NAME = ":lib:p:proc[integer]:x";
    end;

    type IncPt1 is protected body
        variable IncCounter : INTEGER := 0;
            -- For shared variable ArchInc
            --   IncCounter'PATH_NAME = "e:ArchInc:IncCounter"
            --   IncCounter'INSTANCE_NAME = "e(a):ArchInc:IncCounter"
            -- For variable ProcInc
            --   IncCounter'PATH_NAME = "e:p1:ProcInc:IncCounter"
            --   IncCounter'INSTANCE_NAME = "e(a):p1:ProcInc:IncCounter"
        procedure increment is
        begin
            -- For shared variable ArchInc
            --   increment'PATH_NAME = "e:ArchInc:increment"
            --   increment'INSTANCE_NAME = "e(a):ArchInc:increment"
            -- For variable ProcInc
            --   increment'PATH_NAME = "e:p1:ProcInc:increment"
            --   increment'INSTANCE_NAME = "e(a):p1:ProcInc:increment"
            IncCounter := IncCounter + 1;
            report IncCounter'PATH_NAME;
            report IncCounter'INSTANCE_NAME;
        end procedure increment;
        impure function get return INTEGER is
        begin
            -- For shared variable ArchInc
            --   get'PATH_NAME = "e:ArchInc:get"
            --   get'INSTANCE_NAME = "e(a):ArchInc:get"
            -- For variable ProcInc
            --   get'PATH_NAME = "e:p1:ProcInc:get"
            --   get'INSTANCE_NAME = "e(a):p1:ProcInc:get"
            return IncCounter;
        end function get;

        procedure check (p, i, s : string) is
        begin
            assert IncCounter'path_name = p report IncCounter'path_name & " != " & p;
            assert IncCounter'instance_name = i report IncCounter'instance_name & " != " & i;
            assert IncPt1'path_name = s report IncPt1'path_name & " != " & s;
        end procedure;

    end protected body IncPt1;
end;

library lib;
use Lib.P.all;
entity E is                                    -- E is the top-level design entity:
                                               -- E'PATH_NAME = ":e:"
                                               -- E'INSTANCE_NAME = ":e(a):"
    generic (G: INTEGER);                      -- G'PATH_NAME = ":e:g"
                                               -- G'INSTANCE_NAME = ":e(a):g"
    port (P: in INTEGER);                      -- P'PATH_NAME = ":e:p"
                                               -- P'INSTANCE_NAME = ":e(a):p"
begin
    assert E'PATH_NAME = ":top:e:";
end entity E;

architecture A of E is
    signal S: BIT_VECTOR (1 to G);             -- S'PATH_NAME = ":e:s"
                                               -- S'INSTANCE_NAME = ":e(a):s"
    signal tmp : natural;
    procedure Proc1 (signal sp1: NATURAL; C: out INTEGER) is
                                               -- Proc1'PATH_NAME = ":e:proc1[natural,integer]:"
                                               -- Proc1'INSTANCE_NAME =:e(a):proc1[natural,integer]:"
                                               -- C'PATH_NAME = ":e:proc1[natural,integer]:c"
                                               -- C'INSTANCE_NAME = ":e(a):proc1[natural,integer]:c"
        variable max: DELAY_LENGTH;            -- max'PATH_NAME = ":e:proc1[natural,integer]:max"
                                               -- max'INSTANCE_NAME = ":e(a):proc1[natural,integer]:max"
    begin
        assert Proc1'PATH_NAME = ":top:e:proc1[natural, integer]:";
        assert Proc1'INSTANCE_NAME = ":top(top):e@e(a):proc1[natural, integer]:";
        assert c'PATH_NAME = ":top:e:proc1[natural, integer]:c";
        assert c'INSTANCE_NAME = ":top(top):e@e(a):proc1[natural, integer]:c";
        assert max'PATH_NAME = ":top:e:proc1[natural, integer]:max";
        assert max'INSTANCE_NAME = ":top(top):e@e(a):proc1[natural, integer]:max";
        -- max := sp1 * ns;
        -- wait on sp1 for max;
        -- c := sp1;
    end procedure Proc1;

    shared variable ArchInc : IncPt1;   -- ArchInc'PATH_NAME = ":e:ArchInc"
                                        -- ArchInc'INSTANCE_NAME = ":e(a):ArchInc"
begin

    p1: process                                -- T'PATH_NAME = ":e:p1:t"
        variable T: INTEGER := 12;             -- T'INSTANCE_NAME = ":e(a):p1:t"

        variable ProcInc : IncPt1;   -- ProcInc'PATH_NAME = ":e:p1:ProcInc"
                                     -- ProcInc'INSTANCE_NAME = ":e(a):p1:ProcInc"
    begin
        assert lib.P'PATH_NAME = ":lib:p:";
        assert lib.P'INSTANCE_NAME = ":lib:p:";
        proc(t);
        assert s'path_name = ":top:e:s";
        assert s'instance_name = ":top(top):e@e(a):s";
        assert t'path_name = ":top:e:p1:t";
        assert t'instance_name = ":top(top):e@e(a):p1:t";

        proc1(tmp, t);

        ProcInc.increment;
        assert ProcInc.get = 1;
        procinc.check(":top:e:p1:procinc:inccounter",
                      ":top(top):e@e(a):p1:procinc:inccounter",
                      ":top:e:p1:procinc:");
        archinc.check(":top:e:archinc:inccounter",
                      ":top(top):e@e(a):archinc:inccounter",
                      ":top:e:archinc:");
        wait;
    end process p1;

    process
        variable T: INTEGER := 12;             -- T'PATH_NAME = ":e::t"
    begin                                      -- T'INSTANCE_NAME = ":e(a)::t"
        assert t'path_name = ":top:e::t" report t'path_name;
        assert t'instance_name = ":top(top):e@e(a)::t";
        wait;
    end process;
end architecture;

entity Bottom is
    generic (GBottom : INTEGER);
    port (PBottom : INTEGER);
end entity Bottom;

architecture BottomArch of Bottom is
    signal SBottom : INTEGER;
begin
    ProcessBottom : process
        variable V : INTEGER;
    begin
        if GBottom = 4 then
            assert V'Simple_Name = "v"
                and V'Path_Name = ":top:b1:b2:g1(4):b3:l1:processbottom:v"
                and V'Instance_Name =
                ":top(top):b1:b2:g1(4):b3:l1@bottom(bottomarch):processbottom:v";
            assert GBottom'Simple_Name = "gbottom"
                and GBottom'Path_Name = ":top:b1:b2:g1(4):b3:l1:gbottom"
                and GBottom'Instance_Name =
                ":top(top):b1:b2:g1(4):b3:l1@bottom(bottomarch):gbottom";
        elsif GBottom = -1 then
            assert V'Simple_Name = "v"
                and V'Path_Name = ":top:l2:processbottom:v"
                and V'Instance_Name =
                ":top(top):l2@bottom(bottomarch):processbottom:v";
            assert GBottom'Simple_Name = "gbottom"
                and GBottom'Path_Name = ":top:l2:gbottom"
                and GBottom'Instance_Name =
                ":top(top):l2@bottom(bottomarch):gbottom";
        end if;
        wait;
    end process ProcessBottom;

end architecture BottomArch;

entity top is end entity;

architecture top of top is
    component BComp is
        generic (GComp : INTEGER);
        port (PComp : INTEGER);
    end component BComp;
    signal S : INTEGER;
begin
    B1 : block
        signal S : INTEGER;
    begin
        B2 : block
            signal S : INTEGER;
        begin
            G1 : for I in 1 to 10 generate
                B3 : block
                    signal S : INTEGER;
                    for L1 : BComp use entity Work.Bottom(BottomArch)
                        generic map (GBottom => GComp)
                        port map (PBottom => PComp);
                begin
                    L1 : BComp generic map (I) port map (S);
                    P1 : process
                        variable V : INTEGER;
                    begin
                        if I = 7 then
                            assert V'Simple_Name = "v"
                                and V'Path_Name = ":top:b1:b2:g1(7):b3:p1:v"
                                and V'Instance_Name=":top(top):b1:b2:g1(7):b3:p1:v";
                            assert P1'Simple_Name = "p1"
                                and P1'Path_Name = ":top:b1:b2:g1(7):b3:p1:"
                                and P1'Instance_Name = ":top(top):b1:b2:g1(7):b3:p1:";
                            assert S'Simple_Name = "s"
                                and S'Path_Name = ":top:b1:b2:g1(7):b3:s"
                                and S'Instance_Name = ":top(top):b1:b2:g1(7):b3:s";
                            assert B1.S'Simple_Name = "s"
                                and B1.S'Path_Name = ":top:b1:s"
                                and B1.S'Instance_Name = ":top(top):b1:s";
                        end if;
                        wait;
                    end process P1;
                end block B3;
            end generate;
        end block B2;
    end block B1;
    L2 : BComp generic map (-1) port map (S);

    E: entity work.E(A) generic map (1) port map (2);
end architecture top;

configuration attr20 of Top is
    for Top
        for L2 : BComp
            use entity Work.Bottom(BottomArch)
                generic map (GBottom => GComp)
                port map (PBottom => PComp);
        end for;
    end for;
end configuration attr20;
