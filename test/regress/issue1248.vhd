LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

entity issue1248 is
end entity;

architecture test of issue1248 is

    TYPE rec_1 IS RECORD
        x : std_logic_vector(1 DOWNTO 0);
    END RECORD;

    TYPE rec_2 IS RECORD
        a : std_logic_vector(3 DOWNTO 0);
        b : std_logic_vector(7 DOWNTO 0);
        m : rec_1;
    END RECORD;

    signal RST_REC_2 : rec_2;
    signal B : std_logic := 'X';

begin
    process
    begin
        assert (RST_REC_2.a = "UUUU");
        assert (RST_REC_2.b = "UUUUUUUU");

        B <= 'Z';

        wait for 1 ns;

        RST_REC_2 <= (
            m      => (OTHERS => (OTHERS => '0')),
            OTHERS => (OTHERS => B)
        );

        wait for 1 ns;

        assert (RST_REC_2.a = "ZZZZ");
        assert (RST_REC_2.b = "ZZZZZZZZ");

        wait;
    end process;

end architecture;