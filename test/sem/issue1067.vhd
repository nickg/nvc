entity issue1067 is
end entity;

architecture test of issue1067 is
    impure function data_length(x : integer) return natural is
    begin
        return 42;
    end function;

    signal s : bit_vector(data_length(1)'range);  -- Error
begin
end architecture;
