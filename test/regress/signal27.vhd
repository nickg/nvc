entity signal27 is
end entity;

architecture test of signal27 is
    type rec is record
        x, y : integer;
        z : bit_vector(1 to 3);
    end record;

    procedure update(signal r : inout rec) is
    begin
        r <= (r.x + 1, r.y + 1, not r.z);
        wait for 1 ns;
    end procedure;

    signal a : integer := 0;
    signal b : integer := 5;
    signal c : bit_vector(1 to 3) := "101";
begin

    p1: process is
    begin
        update(r.x => a, r.y => b, r.z => c);
        assert a = 1;
        assert b = 6;
        assert c = "010";

        update(r.x => b, r.y => a, r.z => c);
        assert a = 2;
        assert b = 7;
        assert c = "101";

        wait;
    end process;

end architecture;
