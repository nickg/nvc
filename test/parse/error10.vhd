entity error10 is
end entity;

architecture test of error10 is
begin
    g: for in 1 to 10 generate    -- Error
    end generate;
end architecture;
