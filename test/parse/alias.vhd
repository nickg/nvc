entity e is end entity;

architecture a of e is
    signal bar, boo : integer;

    function func (x : integer; b : boolean) return boolean;
    function func return integer;

    procedure proc (x : integer);
    procedure proc;

    alias foo is bar;
    alias blah : integer is boo;

    alias funci is func [integer, boolean return boolean];
    alias proci is proc [integer];
    alias proce is proc [];
    alias funce is func [return integer];
begin

end architecture;
