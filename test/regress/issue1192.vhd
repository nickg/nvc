entity issue1192 is
end entity;

architecture test of issue1192 is
    type t_int_ptr is access integer;

    type t_rec;
    type t_rec_ptr is access t_rec;

    type t_rec is record
        x, y : integer;
    end record;

    type t_file is file of integer;
begin

end architecture;
