entity  SUB is
    port (
        USER_I : in  bit_vector(1 downto 0);
        RESULT : out boolean
    );
end SUB;
architecture MODEL of SUB is
    procedure match(user:in bit_vector; ok: out boolean) is begin
        ok := (user(USER_I'range) = USER_I);
    end procedure;
begin
    process(USER_I)
        variable ok   : boolean;
        constant user : bit_vector(1 downto 0) := "01";
    begin
        match(user, ok);
        RESULT <= ok;
    end process;
end MODEL;

entity  issue217 is
end     issue217;
architecture MODEL of issue217 is
    signal USER : bit_vector(1 downto 0);
    signal OK   : boolean;
begin
    U: entity WORK.SUB port map (
        USER_I => USER,
        RESULT => OK
        );

    user <= "01";

    process is
    begin
        assert not ok;
        wait on ok;
        assert ok;
        wait;
    end process;

end MODEL;
