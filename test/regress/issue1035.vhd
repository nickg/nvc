entity issue1035 is
end entity;

architecture tb of issue1035 is
  type unconstrainedRecord is record
    a : bit;
    b : bit_vector;
  end record;
  subtype uRecConstrained is unconstrainedRecord
    (
      b(1 downto 0)
    );

  signal uRec : unconstrainedRecord(b(1 downto 0));
  signal uRecC: uRecConstrained;
begin
end tb;
