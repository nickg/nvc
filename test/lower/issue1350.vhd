package pkg is
    procedure gen_proc
        generic   (constant n, q : positive; constant r : positive := n)
        parameter (constant v : bit_vector(n-1 downto 0));
end package pkg;

package body pkg is
    procedure gen_proc
        generic   (constant n, q : positive; constant r : positive := n)
        parameter (constant v : bit_vector(n-1 downto 0))
    is begin
        for i in n-1 downto 0 loop
            assert(v(i) = '0');
        end loop;
    end procedure gen_proc;
end package body;

----

use work.pkg.all;

entity ent is begin end entity ent;

architecture arch of ent is
    impure function func return integer is
    begin
        -- Should only be called once
        return 42;
    end function;

    constant nn : positive := 1 + 1;    -- Does not fold
    procedure proc is new gen_proc generic map (n => nn, q => func);
begin
    call: proc("00");
end architecture arch;
