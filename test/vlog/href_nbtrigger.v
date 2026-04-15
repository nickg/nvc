// Event triggers with hierarchical paths
module sub;
  event ev;
endmodule

module href_nbtrigger;

  sub u();

  initial begin
    -> u.ev;                    // OK
    ->> u.ev;                   // OK
    -> ;                        // Error
  end

endmodule // href_nbtrigger
