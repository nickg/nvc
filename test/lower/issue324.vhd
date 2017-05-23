-- test_ng.vhd
entity  TEST is
    generic (
        DATA_BITS   : integer := 8
    );
    port (
        I_DATA      : in  bit_vector(8*DATA_BITS-1 downto 0);
        O_DATA      : out bit_vector(  DATA_BITS-1 downto 0)
    );
end     TEST;
architecture MODEL of TEST is
    type      WORD_VECTOR  is array (INTEGER range <>) of bit_vector(DATA_BITS-1 downto 0);

    function  first_word(WORDS: WORD_VECTOR) return bit_vector is
        alias    i_words :  WORD_VECTOR(0 to WORDS'length-1) is WORDS;
        variable result  :  bit_vector(DATA_BITS-1 downto 0);
    begin
        result := i_words(0);
        return result;
    end function;

begin
    process (I_DATA)
        variable    in_words : WORD_VECTOR(0 to 7);
    begin
        for i in in_words'range loop
            in_words(i) := I_DATA((i+1)*DATA_BITS-1 downto i*DATA_BITS);
        end loop;
        O_DATA  <= first_word(in_words);
    end process;
end MODEL;

entity  TEST_NG is
end     TEST_NG;
architecture MODEL of TEST_NG is
    component TEST is
        generic (
            DATA_BITS    : integer := 8
        );
        port (
            I_DATA       : in  bit_vector(8*DATA_BITS-1 downto 0);
            O_DATA       : out bit_vector(  DATA_BITS-1 downto 0)
        );
    end component;
    constant  DATA_BITS  : integer := 8;
    signal    I_DATA     : bit_vector(8*DATA_BITS-1 downto 0);
    signal    O_DATA     : bit_vector(  DATA_BITS-1 downto 0);
begin
    DUT:TEST generic map(
            DATA_BITS   => DATA_BITS
        )
        port map (
            I_DATA      => I_DATA      ,
            O_DATA      => O_DATA
        );
end     MODEL;
