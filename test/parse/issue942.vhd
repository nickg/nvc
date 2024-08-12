entity test is
end entity test;

architecture rtl of test is
begin
  process(all)

    procedure test_func (
    ) is                                -- Crash after error here
    begin
    end procedure test_func ;
  begin
    wait;
  end process;

end architecture rtl;
