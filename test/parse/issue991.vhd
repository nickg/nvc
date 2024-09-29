package pack is
    type t_enum is (a, b, c);
end package;

-------------------------------------------------------------------------------

entity issue991 is
end entity;

architecture test of issue991 is
    constant c : work.pack.t_enum := work.pack.a;      -- OK
begin
end architecture;
