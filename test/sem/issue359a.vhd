-- this test program aborts during analysis

entity nvc_bug is
end nvc_bug;

architecture behav of nvc_bug is
	signal host_write : bit;
begin

	process
		procedure host_write is
		begin
		end host_write;
	begin
		host_write <= '1';
	end process;

end behav;
