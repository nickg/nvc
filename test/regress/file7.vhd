--
-- Reduced from VESTs cases tc640 and tc641
--

entity file7 is
end entity;

architecture test of file7 is
    subtype word is bit_vector(0 to 15);
    constant size : integer        := 7;
    type primary_memory is array (0 to size) of word;
    type primary_memory_file is file of primary_memory;
    constant C38  : word           := (others => '1');
    constant C44  : primary_memory := (others => C38);
begin
    writer: process
        file filein : primary_memory_file open write_mode is "iofile.42";
    begin
        for i in 1 to 10 loop
            write(filein, C44);
        end loop;
        file_close(filein);
        wait;
    end process;

    reader: process
        file filein : primary_memory_file;
        variable v  : primary_memory;
    begin
        wait for 1 ns;
        file_open(filein, "iofile.42");
        for i in 1 to 10 loop
            assert not endfile(filein);
            read(filein,v);
            assert v = C44;
        end loop;
        wait;
    end process;
end architecture;
