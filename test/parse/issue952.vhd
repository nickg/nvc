entity issue952 is
end entity;

architecture test of issue952 is
    signal x : integer;
begin
    assert << constant ^.foo : integer >> = 2;  -- VHDL
    assert (x = 1 -> x = 2);            -- PSL
    assert x = 1 -> next x = 2;         -- PSL
    assert (<< signal issue952.x : integer >> = 4 -> next x =2);  -- PSL
end architecture;
