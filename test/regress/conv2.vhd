entity sub is
    port ( o1 : out bit;
           o2 : out real := 0.0;
           i1 : in bit );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        o1 <= '1';
        o2 <= 1.0;
        wait for 1 ns;
        o1 <= '0';
        o2 <= 0.0;
        assert i1 = '1';
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity conv2 is
end entity;

architecture test of conv2 is
    type t is (zero, one);

    signal x : real;
    signal y : bit;
    signal z : integer := 0;
    signal p : real;
    signal q : real := 0.0;
    signal r : t;

    type tmap_t is array (t) of bit;

    constant tmap : tmap_t := (zero => '0', one => '1');

    function table_to_bit(x : t) return bit is
    begin
        report "table_to_bit " & t'image(x);
        return tmap(x);
    end function;

    function bit_to_real(b : bit) return real is
    begin
        report "bit_to_real " & bit'image(b);
        if b = '1' then
            return 1.0;
        else
            return 0.0;
        end if;
    end function;

    function real_to_bit(r : real) return bit is
    begin
        report "real_to_bit " & real'image(r);
        if r = 1.0 then
            return '1';
        elsif r = 0.0 then
            return '0';
        else
            report "invalid real value " & real'image(r) severity failure;
        end if;
    end function;

    function bit_to_int(b : bit) return integer is
    begin
        report "bit_to_int " & bit'image(b);
        if b = '1' then
            return integer'high;
        else
            return integer'low;
        end if;
    end function;

    function real_to_limit(r : real) return real is
    begin
        report "real_to_limit " & real'image(r);
        if r = 1.0 then
            return real'high;
        elsif r = 0.0 then
            return real'low;
        else
            report "invalid real value " & real'image(r) severity failure;
        end if;
    end function;

begin

    uut1: entity work.sub
        port map ( bit_to_real(o1) => x,
                   real_to_bit(o2) => y,
                   i1 => real_to_bit(q) );

    uut2: entity work.sub
        port map ( bit_to_int(o1) => z,
                   real_to_limit(o2) => p,
                   i1 => table_to_bit(r) );

    p2: process is
    begin
        assert x = 0.0;
        assert y = '0';
        assert z = integer'low;
        assert p = real'low;
        wait for 0 ns;
        q <= 1.0;
        r <= one;
        assert x = 1.0;
        assert y = '1';
        assert z = integer'high;
        assert p = real'high;
        wait for 2 ns;
        assert x = 0.0;
        assert y = '0';
        assert z = integer'low;
        assert p = real'low;
        wait;
    end process;

end architecture;
