`timescale 1ps/1ps        // OK
`timescale 1ps/1hello     // Error
`timescale 5s/1ps         // Error
`timescale 0.5ps/hello 5  // Error

module foo;
endmodule // foo
