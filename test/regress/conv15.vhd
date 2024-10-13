entity conv15 is
end entity;

architecture test of conv15 is
    type uint8 is range 0 to 255;

    signal s : uint8;
    signal t : bit;

    function to_bit (x : uint8) return bit is
    begin
        assert uint8'pos(x) >= 0;
        if x > 127 then
            return '1';
        else
            return '0';
        end if;
    end function;
begin

    b: block is
        port ( i : in bit; o : out bit );
        port map ( i => to_bit(s), o => t );
    begin
        o <= i;
    end block;

    stim: process is
    begin
        s <= 200;
        wait for 1 ns;
        assert t = '1';
        s <= 100;
        wait for 1 ns;
        assert t = '0';
        s <= 255;
        wait for 1 ns;
        assert t = '1';
        wait;
    end process;

end architecture;
