entity issue1249 is
end entity;

architecture test of issue1249 is
    type int_ptr is access integer;

    procedure test (x : string; variable y : int_ptr) is
    begin
    end procedure;

    procedure test (x : string; y : bit_vector) is
    begin
    end procedure;

begin
    process is
        type line is access string;
        type line_vector is array (0 to 1) of line;
        variable multiple_strings : line_vector := (new string'("file.txt"), new string'("file12.txt"));
    begin
        test(to_string(1), new integer'(1));
        wait;
    end process;
end architecture;
