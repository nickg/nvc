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

entity test is
    port(
        A,B,C : in bit_vector(7 downto 0);
        D : out bit_vector(7 downto 0)
        );
end test;

architecture rtl of test is
begin
    ADD_GEN: for I in 0 to 7 generate
        L: if I=0 generate--failure is here
            U0: entity work.t1
                port map(A(I),B(I),'0',D(I));
        end generate L;
        U: if I>0 generate
            UX: entity work.t1
                port map(A(I),B(I),C(I-1),D(I));
        end generate U;
    end generate ADD_GEN;
end rtl;

entity issue111 is
end entity;

architecture test of issue111 is
    signal A, B, C : bit_vector(7 downto 0);
    signal D       : bit_vector(7 downto 0);
begin
    uut: entity work.test
        port map (
            A => A,
            B => B,
            C => C,
            D => D );

    process is
    begin
        wait for 1 ns;
        assert D = X"00";
        A <= X"ff";
        wait for 1 ns;
        assert D = X"00";
        B <= X"0f";
        C <= X"0c";
        wait for 1 ns;
        assert D = X"08";
        wait;
    end process;

end architecture;
