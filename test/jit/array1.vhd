package array1 is
    type iv is array (natural range <>) of integer;

    procedure assign (a : out iv; b : in iv);
    function get_ints (n, start : integer) return iv;
    function issue94 (dataw : integer; shiftw : integer) return bit_vector;
    procedure test2 (value : string);
end package;

package body array1 is

    procedure assign (a : out iv; b : in iv) is
    begin
        a := b;
    end procedure;

    function get_ints (n, start : integer) return iv is
        variable v : iv(1 to n);
    begin
        for i in 1 to n loop
            v(i) := start + i - 1;
        end loop;
        return v;
    end function;

    function issue94 (dataw : integer; shiftw : integer) return bit_vector is
        constant max_shift : integer := shiftw;

        type bit_vector_array is array (natural range <>) of bit_vector(dataw-1 downto 0);
        variable y_temp : bit_vector_array (0 to max_shift);
    begin
        y_temp(0):=(others=>'1');
        y_temp(1):=(others => '0');
        return y_temp(0);
    end function;

    procedure test2 (value : string) is
        variable x : string(1 to value'length) := value;
    begin
        report x;
    end procedure;
end package body;
