architecture e of a is
begin

    -- Test corner case in lexer
    --   http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
    
    x <= bit_vector'('1','2','3');

    y <= bit'('1');
    
end architecture;
