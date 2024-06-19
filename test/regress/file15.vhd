entity file15 is
end entity;

architecture test of file15 is
    type t_rec is record
        num : natural;
        str : string(1 to 5);
    end record;

    type t_file is file of t_rec;

    type t_strtab is array (natural range <>) of string(1 to 5);

    constant strtab : t_strtab(1 to 5) := (
        "hello",
        "world",
        "12345",
        "hello",
        "donut" );
begin

    testing: process is
        file f : t_file;
        variable r : t_rec;
    begin
        file_open(f, "out", write_mode);
        for i in strtab'range loop
            write(f, (num => i, str => strtab(i)));
        end loop;
        file_close(f);

        file_open(f, "out", read_mode);
        for i in strtab'range loop
            read(f, r);
            assert r.num = i;
            assert r.str = strtab(i) report r.str;
        end loop;
        file_close(f);
        wait;
    end process;

end architecture;
