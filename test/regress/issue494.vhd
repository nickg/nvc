entity issue494 is
end entity;

architecture test of issue494 is
    type ft is file of integer;
    file f : ft;
begin
    p1: process is
        variable status : file_open_status;
    begin
        file_open(status, f, "");
        assert status = NAME_ERROR;
        file_open(status, f, "not_here");
        assert status = NAME_ERROR;
        file_open(f, "");               -- Error
        wait;
    end process;
end architecture;
