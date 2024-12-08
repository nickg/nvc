entity adderN is
    port(addend: in boolean);
end;

architecture struct_adderN of adderN is
    component adder is
        port(addend: in boolean);
    end component;
begin
    u: adder port map(addend'last_value => addend);
end;
