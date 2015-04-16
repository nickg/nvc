entity issue88 is
end entity;

architecture test of issue88 is

    type str_ptr is access string;

    type rec is record
        p : str_ptr;
    end record;

    procedure get_length(variable r : rec; l : out integer) is
    begin
        l := r.p'length;              -- OK
    end;

    procedure get_length2(variable r : rec; l : out integer) is
    begin
        l := r.p.all'length;           -- OK
    end;

begin
end architecture;
