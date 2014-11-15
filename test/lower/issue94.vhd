entity issue94 is
end entity;

architecture test of issue94 is

    function func (dataw : integer; shiftw : integer) return bit_vector is
        constant max_shift : integer := shiftw;

        type bit_vector_array is array (natural range <>) of bit_vector(dataw-1 downto 0);
        variable y_temp : bit_vector_array (0 to max_shift);
    begin
        y_temp(0):=(others=>'1');     -- Error with LLVM asserts build
        return y_temp(0);
    end func;

begin

end architecture;
