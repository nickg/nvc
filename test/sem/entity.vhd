entity e is
    attribute foo : integer;
    attribute foo of e : entity is 55;
end entity;

architecture test of e is
begin

    process is
    begin
        report integer'image(e'foo);      -- OK
    end process;

end architecture;
