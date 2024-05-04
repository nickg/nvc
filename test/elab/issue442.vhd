entity  SAMPLE is
    generic (
        MIN_NUM : integer := 0;
        MAX_NUM : integer := 7
    );
    port (
        I_PORT_E : in  bit_vector(MIN_NUM to MAX_NUM);
        O_PORT_E : out bit_vector(MIN_NUM to MAX_NUM)
    );
end SAMPLE;

architecture SAMPLE_ARCH_1 of SAMPLE is
begin
end SAMPLE_ARCH_1;

entity  ISSUE442 is
end     ISSUE442;
architecture MODEL of ISSUE442 is
    signal    I_PORT_S   :  bit_vector(1 to 4);
    signal    O_PORT_S   :  bit_vector(1 to 4);
    component SAMPLE
        generic (
            MIN_NUM     : integer := 0;
            MAX_NUM     : integer := 7
        );
        port (
            I_PORT_C      : in  bit_vector(MIN_NUM to MAX_NUM);
            O_PORT_C     : out bit_vector(MIN_NUM to MAX_NUM)
        );
    end component;
begin
    DUT: SAMPLE
        generic map(MIN_NUM=>1, MAX_NUM=>4)
        port map(I_PORT_C=>I_PORT_S,O_PORT_C=>O_PORT_S);
end MODEL;

configuration ISSUE442_1 of ISSUE442 is
    for MODEL
        for DUT : SAMPLE
            use entity WORK.SAMPLE(SAMPLE_ARCH_1)
                port map (I_PORT_E => I_PORT_C, O_PORT_E => O_PORT_C);
        end for;
    end for;
end ISSUE442_1;
