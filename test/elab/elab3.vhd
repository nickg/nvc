package p is

    function log2(x : in integer) return integer;

end package;

package body p is

    function log2(x : in integer) return integer is
        variable r : integer := 0;
        variable c : integer := 1;
    begin
        if x <= 1 then
            r := 1;
        else
            while c < x loop
                r := r + 1;
                c := c * 2;
            end loop;
        end if;
        return r;
    end function;

end package body;

-------------------------------------------------------------------------------

entity sub is
    generic ( W : integer );
end entity;

use work.p.all;

architecture test of sub is

    constant B : integer := log2(W);

    signal s : bit_vector(B - 1 downto 0);

    constant C : bit_vector(log2(B) to 1) := (others => '0');

begin

end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
begin

    s : entity work.sub
        generic map ( 10 );


end architecture;

-------------------------------------------------------------------------------

use work.p.all;

entity top2 is
end entity;

architecture test of top2 is
    constant W : integer := 10;
    constant B : integer := log2(W);

    signal s : bit_vector(B - 1 downto 0);
begin

end architecture;
