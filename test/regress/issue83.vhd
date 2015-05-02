
entity read_output is
  port (
    output1, output2 : out integer);
end entity;

architecture a of read_output is
begin
    output1 <= 5;
    output2 <= output1 after 1 ns;
end architecture;

-------------------------------------------------------------------------------

entity issue83 is
end entity;

architecture test of issue83 is
    signal x, y : integer;
begin

    sub: entity work.read_output
        port map ( x, y );

end architecture;
