entity  issue349 is
end     issue349;
architecture RTL of issue349 is
    type      WORD_TYPE    is record
              DATA         :  bit_vector(7 downto 0);
              VAL          :  boolean;
    end record;
    constant  WORD_NULL    :  WORD_TYPE := (DATA => (others => '0'),
                                            VAL  => TRUE);
    type      WORD_VECTOR  is array (INTEGER range <>) of WORD_TYPE;
    signal    curr_queue   :  WORD_VECTOR(0 to 1);
begin
    curr_queue <= (others => WORD_NULL);

    process is
    begin
        wait for 1 ns;
        assert curr_queue = (0 to 1 => WORD_NULL);
        wait;
    end process;

end RTL;
