entity external is
end entity;

architecture test of external is
begin

    main: process is
        variable i : integer;
    begin
        i := <<signal foo.bar : integer>>;  -- OK
        i := << constant x.y.z : integer >>;  -- OK
        i := <<variable aye.bee : integer>>;  -- OK
        i := << constant .x.y.z : integer>>;  -- OK
        i := << constant ^.^.foo : integer >>;  -- OK
        i := << constant @work.pack.foo : integer >>;  -- OK
        i := << signal g(0).x(1).baz : integer >>;  -- OK
    end process;

    p2: process is
        alias sig is <<signal i_test.sig : bit_vector(1 downto 0)>>;
    begin
        sig(0) <= '1';                  -- OK
    end process;

end architecture;
