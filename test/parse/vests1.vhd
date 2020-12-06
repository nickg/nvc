configuration testbench of something is
    for arch
    end for;
end;

configuration c01s03b01x00p12n01i00863cfg of c01s03b01x00p12n01i00863ent is
  for c01s03b01x00p12n01i00863arch
    for K
      for T5:test use configuration work.testbench;
      end for;
      for G(3)
        for T1:test
          use configuration work.testbench;
        end for;
      end for;
      for G(0 to 2)
        for all:test
          use configuration work.testbench;
        end for;
      end for;
    end for;
  end for;
end;
