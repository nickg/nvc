package a is
    constant W : natural := 9;
end package a;

use work.a.all;

entity opt is
    port (
        o : out bit_vector(W-1 downto 0)
    );
end entity opt;

architecture rtl of opt is
    signal size_8 : bit_vector(7 downto 0);
    signal size_9 : bit_vector(8 downto 0);
begin

    o <= size_9 when W = 9 else size_8;

    process (size_8, size_9)
    begin
        if W = 9 then
            o <= size_9;
        else
            o <= size_8;
        end if;
    end process;

end architecture rtl;
