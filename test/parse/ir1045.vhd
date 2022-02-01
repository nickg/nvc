entity e is end entity;

architecture a of e is
    signal x : bit_vector(1 to 3);
    signal y : bit;
begin

    -- Test corner case in lexer
    --   http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt

    x <= bit_vector'('1','0','1');

    y <= bit'('1');

end architecture;
