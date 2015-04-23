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

    type str_ptr_ptr is access str_ptr;

    type rec2 is record
        pp : str_ptr_ptr;
    end record;

    procedure get_length3(variable r : rec2; l : out integer) is
    begin
        l := r.pp.all'length;           -- OK
        l := r.p.all.all'length;        -- Error
    end;

begin
end architecture;
