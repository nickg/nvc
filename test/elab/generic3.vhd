entity sub is
    generic ( i : integer;
              s : string;
              t : time;
              b : bit_vector;
              type ty );
end entity;

architecture test of sub is
    signal x : string(1 to 2) := s;  -- To trigger error
begin
end architecture;

-------------------------------------------------------------------------------

entity generic3 is
end entity;

architecture test of generic3 is
begin

    u: entity work.sub
        generic map ( 42,
                      "hello",
                      5 ns,
                      ('0', '1', '0'),
                      integer );

end architecture;
