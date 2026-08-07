package indexable_constraints_pkg is
    type word_t is array (natural range <>) of integer;
    type matrix_t is array (natural range <>) of word_t;

    function make_words return word_t;

end package;

package body indexable_constraints_pkg is
    function make_words return word_t is
    begin
        return (10, 20, 30, 40);
    end function;
end package body;

-------------------------------------------------------------------------------

use work.indexable_constraints_pkg.all;

entity issue1632 is
    generic (
        N : positive := 4
    );
    port (
        word_o   : out word_t(N - 1 downto 0);
        matrix_o : out matrix_t(N - 1 downto 0)(N - 1 downto 0)
    );
end entity;

architecture test of issue1632 is
    constant C_from_func : word_t := make_words;       -- BROKEN
    constant C_from_list : word_t := (10, 20, 30, 40); -- WORKS
    constant C_tmp       : word_t := make_words;       -- BROKEN
    constant C_forwarded : word_t := C_tmp;            -- BROKEN
begin

    word_o   <= C_from_func;                      -- Forward: WORKS
    matrix_o <= (word_o, word_o, word_o, word_o); -- Forward: WORKS

end architecture;
