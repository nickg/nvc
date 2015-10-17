entity e1 is
end entity;

architecture a1 of e1 is

    type t_protected is protected
    end protected;
    type t_protected is protected body
    end protected body;

    type bad_file_type is file of t_protected;   -- Error

    type t_protected_array  is array (0 to 1) of t_protected;   -- Error

    type t_protected_record is record
        a   : integer;
        b   : t_protected;              -- Error
        c   : real;
    end record;

begin

end architecture;
