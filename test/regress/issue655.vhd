library ieee ;
    use ieee.numeric_std.all ;

entity saturate is
  port (
    x   :   in  signed(31 downto 0) ;
    y   :   out signed(15 downto 0)
  ) ;
end entity ;

architecture ifarch of saturate is

    constant sat_high : signed := to_signed(2**(y'length-1) - 1, y'length) ;
    constant sat_low  : signed := to_signed(-(2**(y'length-1)), y'length) ;

    constant too_high : integer := to_integer(sat_high) ;
    constant too_low  : integer := to_integer(sat_low) ;

begin

    process(all)
    begin
        if( x >= too_high ) then
            y <= sat_high ;
        elsif( x <= sat_low ) then
            y <= sat_low ;
        else
            y <= resize(x, y'length) ;
        end if ;
    end process ;

end architecture ;

architecture casearch of saturate is

    subtype too_high is integer range 2**(y'length-1)-1 to integer'high ;
    subtype too_low is integer range integer'low to -(2**(y'length-1)) ;

    constant sat_high : signed := to_signed(2**y'length - 1, y'length) ;
    constant sat_low : signed := to_signed(0, y'length) - sat_high - to_signed(1, y'length) ;

begin

    process(all)
    begin
        case to_integer(x) is
            when too_high => y <= sat_high ;
            when too_low  => y <= sat_low ;
            when others   => y <= resize(x, y'length) ;
        end case ;
    end process ;

end architecture ;

entity issue655 is
end entity;

library ieee ;
    use ieee.numeric_std.all ;
    use ieee.std_logic_1164.all ;

architecture test of issue655 is
    signal x : signed(31 downto 0) := X"00000000";
    signal y1, y2 : signed(15 downto 0);
begin

    u1: entity work.saturate(ifarch) port map (x, y1);

    u2: entity work.saturate(ifarch) port map (x, y2);

    p1: process is
    begin
        x <= to_signed(-(2 ** 30), 32);
        wait for 1 ns;
        assert y1 = X"8000" report to_hstring(y1);
        assert y2 = X"8000" report to_hstring(y2);
        x <= to_signed(55, 32);
        wait for 1 ns;
        assert y1 = X"0037" report to_hstring(y1);
        assert y2 = X"0037" report to_hstring(y2);
        x <= to_signed(2 ** 24, 32);
        wait for 1 ns;
        assert y1 = X"7fff" report to_hstring(y1);
        assert y2 = X"7fff" report to_hstring(y2);
        wait;
    end process;

end architecture;
