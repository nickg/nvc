ENTITY dummy IS PORT(
    i : IN bit;
    o : OUT bit );
END ENTITY dummy;

ARCHITECTURE arch OF dummy IS
BEGIN
    o <= i;
END ARCHITECTURE arch;

ENTITY issue104 IS
END ENTITY issue104;

ARCHITECTURE arch OF issue104 IS
    SIGNAL v : bit_vector(4 DOWNTO 0);
BEGIN
    fold : FOR i IN 0 TO 3 GENERATE
    BEGIN
        dummy : ENTITY work.dummy PORT MAP(
            i => v(i+1),
            o => v(i)
        );
    END GENERATE fold;

    process is
    begin
        v(4) <= '1';
        wait for 1 ns;
        assert v(0) = '1';
        wait;
    end process;

END ARCHITECTURE arch;
