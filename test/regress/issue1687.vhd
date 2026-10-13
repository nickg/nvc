entity issue1687 is
end entity;

architecture arch of issue1687 is
begin
    assert real'value("1.0e01")  = 10.0;
    assert real'value("2.0e1 ")  = 20.0;
    assert real'value("3.0e-01") =  0.3;
end architecture;
