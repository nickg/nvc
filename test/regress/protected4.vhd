entity protected4 is
end entity;

architecture test of protected4 is

    type p is protected
        procedure init(path : string);
    end protected;

    type p is protected body
        type ft is file of integer;
        file f : ft;

        procedure init(path : string) is
        begin
            file_open(f, path, WRITE_MODE);
        end procedure;
    end protected body;

    procedure run_test is
        variable x : p;
    begin
        x.init("data");
    end procedure;
begin

    process is
    begin
        run_test;
        wait;
    end process;

end architecture;
