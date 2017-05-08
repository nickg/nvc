entity issue323 is
end entity;

architecture test of issue323 is
    type intfile is protected
        procedure iwrite(x : in integer);
        procedure iread(x : out integer);
    end protected;

    type intfile is protected body
        type ft is file of integer;
        file f : ft;
        constant FNAME : string := "data";

        procedure iwrite(x : in integer) is
        begin
            file_open(f, FNAME, WRITE_MODE);
            write(f, x);
            file_close(f);
        end procedure;

        procedure iread(x : out integer) is
        begin
            file_open(f, FNAME, READ_MODE);
            read(f, x);
            file_close(f);
        end procedure;
    end protected body;

    shared variable f : intfile;
begin

    p0: process is
    begin
        f.iwrite(42);
        wait;
    end process;

    p1: process is
        variable x : integer;
    begin
        wait for 1 ns;
        f.iread(x);
        assert x = 42;
        wait;
    end process;

end architecture;
