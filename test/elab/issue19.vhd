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

function cfunc2 (constant k : integer) return integer is
    variable tmp : integer;
  begin
    tmp := 1;
   for i in 0 to k loop
      if tmp > k then
        return i;
   end if;
      tmp := tmp + tmp;
    end loop;
end cfunc2;

function my_cfunc2 (constant k: integer) return integer is
begin
    if k > 1 then
        return cfunc(k);
    end if;
    return 1;
end my_cfunc2;

constant cnum : integer := cfunc(num);
type m_a_t is array (cnum-1 downto 0) of bit_vector(num-1 downto 0);
signal ma : m_a_t;
signal tmp : integer := cnum;

constant cnum2 : integer := cfunc2(num);
type m_a_t2 is array (cnum2-1 downto 0) of bit_vector(num-1 downto 0);
signal ma2 : m_a_t2;
signal tmp2 : integer := cnum2;

constant cnum3 : integer := my_cfunc2(num);
type m_a_t3 is array (cnum3-1 downto 0) of bit_vector(num-1 downto 0);
signal ma3 : m_a_t3;
signal tmp3 : integer := cnum3;

begin
    y <= x;

    g1: if cnum /= 32 generate
        assert false;
    end generate;

    g2: if cnum3 /= 32 generate
        assert false;
    end generate;

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
