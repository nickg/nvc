entity split1 is
end entity;

architecture test of split1 is
    signal s : bit_vector(7 downto 0);

    procedure split(signal o : out bit_vector; idx : in integer) is
    begin
        o(idx) <= '1';
    end procedure;
begin

    p1: process is
    begin
        s <= force X"12";
        split(s, 4);
        wait;
    end process;

end architecture;
