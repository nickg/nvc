entity issue978 is
end entity;

architecture test of issue978 is
    constant period : delay_length := 20 ns;  -- VHPI error here

    type t_int64 is range -9223372036854775807 - 1 to 9223372036854775807;

    constant one : t_int64 := 1;

    subtype t_sub is t_int64 range 1 to 100;

    constant two : t_sub := 42;
begin
end architecture;
