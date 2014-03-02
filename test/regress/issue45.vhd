entity issue45 is
begin
end entity issue45;

architecture a of issue45 is

begin

    b: block is
    begin
        p : process
        begin
            report p'instance_name;
            report b'instance_name;
            wait;
        end process p;
    end block;

end architecture a;
