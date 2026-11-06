module force5;
  reg driver;
  wire net_value = driver;
  reg variable;
  reg [3:0] vector_driver;
  wire [3:0] net_vector = vector_driver;
  reg [3:0] variable_vector;
  reg failed;

  initial begin
    failed = 0;
    driver = 0;
    variable = 0;
    vector_driver = 0;
    variable_vector = 0;
    #1;

    force net_value = 1;
    force variable = 1;

    if (net_value !== 1 || variable !== 1) begin
      $display("immediate force: net=%b variable=%b", net_value, variable);
      failed = 1;
    end

    driver = 0;
    variable = 0;
    #1;

    if (net_value !== 1 || variable !== 1) begin
      $display("force: net=%b variable=%b", net_value, variable);
      failed = 1;
    end

    variable = 0;
    #1;

    if (variable !== 1) begin
      $display("assignment while forced: variable=%b", variable);
      failed = 1;
    end

    variable <= 0;
    #1;

    if (variable !== 1) begin
      $display("nonblocking assignment while forced: variable=%b", variable);
      failed = 1;
    end

    release net_value;
    release variable;

    if (net_value !== 0 || variable !== 1) begin
      $display("immediate release: net=%b variable=%b", net_value, variable);
      failed = 1;
    end

    #1;

    // Releasing a net restores its driven value, while a variable keeps the
    // forced value until the next procedural assignment.
    if (net_value !== 0 || variable !== 1) begin
      $display("release: net=%b variable=%b", net_value, variable);
      failed = 1;
    end

    variable = 0;
    #1;

    if (variable !== 0) begin
      $display("assignment after release: variable=%b", variable);
      failed = 1;
    end

    force variable = 1;
    #1;
    release variable;
    variable = 0;
    #1;

    if (variable !== 0) begin
      $display("assignment immediately after release: variable=%b", variable);
      failed = 1;
    end

    force variable = 1;
    release variable;
    #1;

    if (variable !== 1) begin
      $display("immediate force/release: variable=%b", variable);
      failed = 1;
    end

    variable = 0;

    force net_vector[2:1] = 2'b11;
    force variable_vector[2:1] = 2'b11;
    vector_driver = 0;
    variable_vector = 0;
    #1;

    if (net_vector !== 4'b0110 || variable_vector !== 4'b0110) begin
      $display("part-select force: net=%b variable=%b",
               net_vector, variable_vector);
      failed = 1;
    end

    release net_vector[2:1];
    release variable_vector[2:1];
    #1;

    if (net_vector !== 0 || variable_vector !== 4'b0110) begin
      $display("part-select release: net=%b variable=%b",
               net_vector, variable_vector);
      failed = 1;
    end

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule
