package p is

    component c is
        generic ( X : integer );
        port ( o : out integer );
    end component;

    component foo
        port ( x : inout integer );
    end component foo;

end package;
