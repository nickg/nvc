entity t is
    generic(
        ORDER : integer := 8
        );
    port(
        clk : in bit;
        reset : in bit
        );
end entity t;
architecture RTL of t is
    function calc_order(i:integer) return integer is
    begin
        if i mod 2 = 1 then
            return i/2+1;
        else
            return i/2;
        end if;
    end function;
    constant C_ORDER :integer:=calc_order(ORDER);
    type t_48 is array (C_ORDER-1 downto 0) of bit_vector(47 downto 0);
    signal a:t_48;
    constant zero48 : bit_vector(47 downto 0):=(others=>'0');
begin
    loop_gen: for i in 0 to C_ORDER-1 generate
        a(i)<=zero48;
    end generate;
end architecture RTL;
