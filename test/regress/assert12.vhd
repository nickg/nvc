entity assert12 is
end entity;

architecture test of assert12 is

    impure function bang (n : integer) return severity_level is
    begin
        if 10 / n > 2 then
            return error;
        else
            return warning;
        end if;
    end function;

    signal s : bit := '1';

begin

    process is
    begin
        wait for 1 ns;
        assert s = '1' severity bang(0);
        wait;
    end process;

end architecture;
