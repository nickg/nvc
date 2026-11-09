entity issue1722 is
end entity;

architecture test of issue1722 is
    constant s_load : bit := bit'('0');
    constant s_proc : bit := bit'('1');

    signal clk : bit := '0';
    signal state : bit := '0';
    signal count : natural := 0;
begin
    p1: process (clk) is
    begin
        if rising_edge(clk) then
            case state is
                when s_load =>
                    state <= s_proc;
                    count <= count + 1;
                when s_proc =>
                    state <= s_load;
                    count <= count + 2;
                when others =>
                    null;
            end case;
        end if;
    end process;
end architecture;
