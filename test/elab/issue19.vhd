entity comp6_bot is
generic (num : integer := 2 );
    port (
        x : in bit_vector(7 downto 0);
        y : out bit_vector(7 downto 0) );
end entity;

architecture rtl of comp6_bot is

 function cfunc (constant val : integer) return integer is
    variable tmp : integer;
	begin    tmp := 0;
	    for i in 0 to 3 loop
	    tmp := tmp + val;
           end loop;
	return tmp;
end function cfunc;

constant cnum : integer := cfunc(num);
type m_a_t is array (cnum-1 downto 0) of bit_vector(num-1 downto 0);
signal ma : m_a_t;
signal tmp : integer := cnum;

begin
    y <= x;
end architecture;

-------------------------------------------------------------------------------

entity comp6 is
end entity;

architecture rtl of comp6 is
signal b: bit_vector(7 downto 0);


    component comp6_bot is
        generic (num : integer := 2 );
        port (
            y : out bit_vector(7 downto 0);
            x : in bit_vector(7 downto 0) );
    end component;
begin

    c1: component comp6_bot
	generic map (num => 8)
        port map ( x=>x"aa", y=>b );

end architecture;
