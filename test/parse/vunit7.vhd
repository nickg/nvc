entity vunit7 is
end entity;

architecture test of vunit7 is

    type log_level_t is (trace, info, error, failure);

    impure function trace (l : log_level_t; x : string) return integer is
    begin
        return 1;
    end function;

    procedure trace (l : log_level_t; x : string) is
    begin
    end procedure;

begin

    p: process is
    begin
        for log_level in trace to failure loop  -- OK
        end loop;
        for log_level in error to failure loop  -- Error
        end loop;
    end process;

end architecture;
