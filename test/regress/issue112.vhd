entity t1 is
    port(
        A,B,C : in bit;
        D : out bit
        );
end t1;

architecture rtl of t1 is
begin
    D<='1' when A='1' and B='1' and C='1' else '0';
end rtl;

entity test2 is
    port(
        A,B,C : in bit_vector(7 downto 0);
        D0 : out bit_vector(7 downto 0);
        D1 : out bit_vector(7 downto 0);
        D2 : out bit_vector(7 downto 0);
        D3 : out bit_vector(7 downto 0);
        D4 : out bit_vector(7 downto 0);
        D5 : out bit_vector(7 downto 0);
        D6 : out bit_vector(7 downto 0);
        D7 : out bit_vector(7 downto 0)
        );
end test2;

architecture rtl of test2 is
    type T_IN_DSP_PC_ARR is array(0 to 7,0 to 7) of bit;
    signal s_pout : T_IN_DSP_PC_ARR;
begin
    D0 <= s_pout(0,7)&s_pout(0,6)&s_pout(0,5)&s_pout(0,4)&s_pout(0,3)&s_pout(0,2)&s_pout(0,1)&s_pout(0,0);
    D1 <= s_pout(1,7)&s_pout(1,6)&s_pout(1,5)&s_pout(1,4)&s_pout(1,3)&s_pout(1,2)&s_pout(1,1)&s_pout(1,0);
    D2 <= s_pout(2,7)&s_pout(2,6)&s_pout(2,5)&s_pout(2,4)&s_pout(2,3)&s_pout(2,2)&s_pout(2,1)&s_pout(2,0);
    D3 <= s_pout(3,7)&s_pout(3,6)&s_pout(3,5)&s_pout(3,4)&s_pout(3,3)&s_pout(3,2)&s_pout(3,1)&s_pout(3,0);
    D4 <= s_pout(4,7)&s_pout(4,6)&s_pout(4,5)&s_pout(4,4)&s_pout(4,3)&s_pout(4,2)&s_pout(4,1)&s_pout(4,0);
    D5 <= s_pout(5,7)&s_pout(5,6)&s_pout(5,5)&s_pout(5,4)&s_pout(5,3)&s_pout(5,2)&s_pout(5,1)&s_pout(5,0);
    D6 <= s_pout(6,7)&s_pout(6,6)&s_pout(6,5)&s_pout(6,4)&s_pout(6,3)&s_pout(6,2)&s_pout(6,1)&s_pout(6,0);
    D7 <= s_pout(7,7)&s_pout(7,6)&s_pout(7,5)&s_pout(7,4)&s_pout(7,3)&s_pout(7,2)&s_pout(7,1)&s_pout(7,0);

    VGEN: for V in 0 to 7 generate
        UGEN: for I in 0 to 7 generate
            signal C_C :bit;
        begin
            process (C) is
            begin
                if I=0 then
                    C_C <= '0';
                else
                    C_C <= C(I-1);
                end if;
            end process;
            UX: entity work.t1
                port map(A(I),B(I),C_C,s_pout(V,I));
        end generate UGEN;
    end generate VGEN;
end rtl;

entity issue112 is
end entity;

architecture test of issue112 is
    signal A, B, C : bit_vector(7 downto 0);
    signal D0      : bit_vector(7 downto 0);
    signal D1      : bit_vector(7 downto 0);
    signal D2      : bit_vector(7 downto 0);
    signal D3      : bit_vector(7 downto 0);
    signal D4      : bit_vector(7 downto 0);
    signal D5      : bit_vector(7 downto 0);
    signal D6      : bit_vector(7 downto 0);
    signal D7      : bit_vector(7 downto 0);
begin
    test2_1: entity work.test2
        port map (
            A  => A,
            B  => B,
            C  => C,
            D0 => D0,
            D1 => D1,
            D2 => D2,
            D3 => D3,
            D4 => D4,
            D5 => D5,
            D6 => D6,
            D7 => D7);

    process is
    begin
        wait for 1 ns;
        assert D0 = X"00";
        A <= X"0f";
        B <= X"03";
        C <= X"01";
        wait for 1 ns;
        assert D0 = X"02";
        wait;
    end process;

end architecture;
