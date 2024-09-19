entity issue978 is
end entity;

architecture test of issue978 is
    constant period : delay_length := 20 ns;  -- VHPI error here
begin
end architecture;
