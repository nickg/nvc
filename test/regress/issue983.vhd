entity foo is
  generic (
      a: natural
  );
end entity foo;

architecture rtl of foo is
begin
    process is
    begin
        assert a = 5;
        wait;
    end process;
end architecture;

entity issue983 is
end entity;

architecture tb of issue983 is
    signal a_actual : natural := 5;
begin

    foo_inst: entity work.foo
    generic map (
        a => a_actual
    );
end architecture tb;
