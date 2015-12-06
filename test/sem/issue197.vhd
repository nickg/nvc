entity entity_attr is
  constant MIN_DELAY : NATURAL := 42;
  attribute DELAY   : NATURAL;
  attribute DELAY of entity_attr : entity is MIN_DELAY;
end entity;

architecture foo of entity_attr is

begin
end architecture;

entity issue197 is
end entity;

architecture foe of issue197 is
    constant fumble: natural := work.entity_attr'DELAY;
begin
alu_div_0:
    entity work.entity_attr ;

MONITOR:
    process
    begin
        assert fumble = 42;
        assert work.entity_attr'DELAY = 42;
        wait;
    end process;
end architecture;
