library ieee;
use ieee.std_logic_1164.all;

entity priority_encoder is
    port (
        inp : in std_logic_vector(2 downto 0);
        outp : out std_logic_vector(2 downto 0));
end priority_encoder;

architecture arch of priority_encoder is
begin
    process(all)
    begin
        case? inp is
        when "1--" => outp <= "100";
            when "01-" => outp <= "010";
            when "001" => outp <= "001";
            when others => outp <= "000";
        end case?;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity issue670 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue670 is
    signal inp : std_logic_vector(2 downto 0);
    signal outp : std_logic_vector(2 downto 0);
begin

    u: entity work.priority_encoder
        port map (inp, outp);

    stim: process is
    begin
        inp <= "100";
        wait for 1 ns;
        assert outp = "100";

        inp <= "110";
        wait for 1 ns;
        assert outp = "100";

        inp <= "010";
        wait for 1 ns;
        assert outp = "010";

        wait;
    end process;

end architecture;
