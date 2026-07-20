entity e is
end entity;

architecture a of e is
  type t is protected
    impure function f return real;
    impure function f return integer;
    impure function g(x : real) return integer;
    impure function g(x : integer) return integer;
  end protected;

  type t is protected body
    impure function f return real is begin return 0.0; end;
    impure function f return integer is begin return 0; end;
    impure function g(x : real) return integer is begin return 0; end;
    impure function g(x : integer) return integer is begin return 0; end;
  end protected body;

  shared variable x : t;
begin
  process is
    variable y : integer;
  begin
    y := x.g(x.f + 25.0);
    wait;
  end process;
end architecture;

entity e2 is
end entity;

architecture a of e2 is
  type t is protected
    impure function count return integer;
    impure function get(i : integer) return integer;
  end protected;

  type t is protected body
    impure function count return integer is begin return 0; end;
    impure function get(i : integer) return integer is begin return i; end;
  end protected body;

  shared variable x : t;
begin
  process is
    procedure add(i : integer) is begin end;
  begin
    for i in 1 to x.count loop
      add(x.get(i));
    end loop;
    wait;
  end process;
end architecture;
