entity e is
    attribute foo : integer;
    attribute foo of e : entity is 55;
    constant c : integer := 1;
begin
    pass : assert (e'foo = 55 and c = 1)  -- OK
           report "unexpected"
           severity failure;
end entity;

package pack is
end package;

architecture test of e is
    constant d : integer := c + 1;      -- OK
begin

    process is
    begin
        report integer'image(e'foo);      -- OK
    end process;

    recur: entity work.e(invalid)       -- OK (until elaboration)
        ;

    bad: entity work.pack;              -- Error

end architecture;

architecture a of pack is               -- Error
begin
end architecture;
