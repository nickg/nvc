package pack1 is
    type t is (A, B, C);
    type t_vec is array (natural range <>) of t;
end package;

package pack1 is
    type t is (C, B, A);                -- Redefine t
    type t_vec is array (1 to 2) of t;
    constant x : boolean := t_vec'(A, A) < t_vec'(C, C);      -- Should not fold!
end package;

entity order1 is
end entity;

use work.pack1.all;

architecture test of order1 is
begin
    main: process is
    begin
        assert x = false;
        wait;
    end process;
end architecture;
