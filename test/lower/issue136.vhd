entity cannot_return_safety_check_pkg is
end entity;

architecture test of cannot_return_safety_check_pkg is
  type record_t is record
    element : natural;
  end record;

  type record_returner_t is protected
    impure function return_record return record_t;
  end protected;

  type record_returner_t is protected body
   variable rec : record_t;
    impure function return_record return record_t is
    begin
      return rec;                       -- Error here
    end;
  end protected body;
begin

end architecture;
