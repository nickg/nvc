entity e is
end entity;

architecture a of e is
    constant x : integer := 5;
begin

    process is
        variable v : integer;
    begin
        v := x;                         -- OK
        x := v;                         -- Error
    end process;

end architecture;
