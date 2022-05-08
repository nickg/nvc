package pack is
    alias resolved_max is maximum [integer_vector return integer] ;

    --function resolved_max (i : integer_vector) return integer;

    subtype RdyType is resolved_max integer range  0 to integer'high ;
    subtype AckType is resolved_max integer range -1 to integer'high ;

    procedure request(signal rdy : inout RdyType;
                      signal ack : in AckType);

    procedure wait_for(signal ack : inout AckType;
                       signal rdy : in RdyType);

    type ctl_t is record
        rdy : RdyType;
        ack : AckType;
    end record;

end package;

package body pack is

    -- function resolved_max (i : integer_vector) return integer is
    -- begin
    --     return maximum(i);
    -- end function;

    procedure request(signal rdy : inout RdyType;
                      signal ack : in AckType) is
    begin
        rdy <= rdy + 1;
        wait for 0 ns;
        wait until rdy = ack;
    end procedure;

    procedure wait_for(signal ack : inout AckType;
                       signal rdy : in RdyType) is
    begin
        ack <= ack + 1;
        wait until ack /= rdy;
    end procedure;

end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( ctl : inout ctl_t );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        wait for 5 ns;
        wait_for(ctl.ack, ctl.rdy);
        report "processing in p2";
        wait for 12 ns;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity wait21 is
end entity;

architecture test of wait21 is
    signal ctl : ctl_t;
    signal done : boolean := false;
begin

    uut: entity work.sub port map (ctl);

    p2: process is
    begin
        request(ctl.rdy, ctl.ack);
        assert now = 22 ns;
        report "acked request";
        wait for 2 ns;
        request(ctl.rdy, ctl.ack);
        assert now = 41 ns;
        done <= true;
        wait;
    end process;

    p3: process is
    begin
        wait for 1 hr;
        assert done = true;
        wait;
    end process;

end architecture;
