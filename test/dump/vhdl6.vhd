entity vhdl6 is
end entity;

architecture test of vhdl6 is
    signal x : integer;
begin
    assert << constant ^.foo : integer >> = 2;  -- VHDL
    assert (x = 1 -> x = 2);            -- PSL
end architecture;
