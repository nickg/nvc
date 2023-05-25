entity samples is
end entity;

architecture test of samples is
   type sample is array (natural range <>) of integer;
   type dozen_samples is array (1 to 12) of sample;
   subtype dozen_short_samples is dozen_samples(open)(0 to 9);

   signal my_dozen: dozen_short_samples;  -- OK
begin
end architecture;
