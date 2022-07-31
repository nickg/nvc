entity toplevel4 is
    generic ( config : string; r : real; t : time );
end entity;

architecture test of toplevel4 is
begin
    assert config = "hello";
    assert r = 2.5;
    assert t = 5 ms;
end architecture;
