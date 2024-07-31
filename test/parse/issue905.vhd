entity const_hides is
    port (
        rst      : in  bit;
        clk      : in  bit;

        i_part   : in  bit;
        o_result : out bit

    );
end entity const_hides;


architecture rtl of const_hides is

    constant C_NB : positive := 4;
    signal partA : bit_vector(C_NB downto 0);
    signal partB : bit_vector(C_NB downto 0);

begin

    partA(0) <= i_part;

    gen_i : for i in 0 to C_NB-1 generate

        proc_i : process(clk, rst)
        begin
            if (rst = '1') then

                partA(i+1) <= '0';

            elsif clk = '1' and clk'event then

                partA(i+1) <= partA(i);

                --loop_part : for k in 0 to i loop
                loop_part : for i in 0 to i loop  -- reusing the same generate loop index by accident 

                    if (partA(i) = '1') then
                        --partB(k+1) <= partB(k);
                        partB(i+1) <= partB(i);
                    end if;

                end loop loop_part;

            end if;
        end process proc_i;

    end generate gen_i;

    o_result <= partB(C_NB);

end architecture rtl;
