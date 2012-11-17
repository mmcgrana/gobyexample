library ieee;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_1164.all;   
use ieee.numeric_std.all;


entity top_testbench is --test
	generic ( -- test
	    n : integer := 8 -- test
	); -- test
end top_testbench; -- test


architecture top_testbench_arch of top_testbench is  

    component top is
        generic (
            n : integer
        )   ;
        port (
            clk : in std_logic;
            rst : in std_logic;
            d1 : in std_logic_vector (n-1 downto 0);
            d2 : in std_logic_vector (n-1 downto 0);
            operation : in std_logic;
            result : out std_logic_vector (2*n-1 downto 0)
        );
    end component;

    signal clk : std_logic;
    signal rst : std_logic;
	signal operation : std_logic;
    signal d1 : std_logic_vector (n-1 downto 0);
    signal d2 : std_logic_vector (n-1 downto 0);
    signal result : std_logic_vector (2*n-1 downto 0);
    
    type test_type is ( a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
    attribute enum_encoding of my_state : type is "001 010 011 100 111";
begin

    TESTUNIT : top generic map (n => n)
                   port map (clk => clk,
                             rst => rst,
                             d1  => d1,
                             d2  => d2,
                             operation => operation,
                             result => result);

    clock_process : process
    begin
        clk <= '0';
        wait for 5 ns;
        clk <= '1';
        wait for 5 ns;
    end process;

    data_process : process
    begin       
		
		-- test case #1	
	   	operation <= '0';
		
        rst <= '1';
        wait for 5 ns;
        rst <= '0';
        wait for 5 ns;
		
		d1 <= std_logic_vector(to_unsigned(60, d1'length));
		d2 <= std_logic_vector(to_unsigned(12, d2'length));
		wait for 360 ns;
		
		assert (result = std_logic_vector(to_unsigned(720, result'length)))
			report "Test case #1 failed" severity error; 
            
		-- test case #2	
	   	operation <= '0';
		
        rst <= '1';
        wait for 5 ns;
        rst <= '0';
        wait for 5 ns;
		
		d1 <= std_logic_vector(to_unsigned(55, d1'length));
		d2 <= std_logic_vector(to_unsigned(1, d2'length));
		wait for 360 ns;
		
		assert (result = std_logic_vector(to_unsigned(55, result'length)))
			report "Test case #2 failed" severity error;
            
        -- etc 
            
    end process;

end top_testbench_arch;


configuration testbench_for_top of top_testbench is
	for top_testbench_arch
		for TESTUNIT : top
			use entity work.top(top_arch);
		end for;
	end for;
end testbench_for_top;


function compare(A: std_logic, B: std_Logic) return std_logic is
    constant pi : real := 3.14159;
    constant half_pi : real := pi / 2.0;
    constant cycle_time : time := 2 ns;
    constant N, N5 : integer := 5;
begin
    if (A = '0' and B = '1') then
        return B;
    else
        return A;
    end if ;
end compare;


procedure print(P : std_logic_vector(7 downto 0);
                U : std_logic_vector(3 downto 0)) is
    variable my_line : line;
    alias swrite is write [line, string, side, width] ;
begin
    swrite(my_line, "sqrt( ");
    write(my_line, P);
    swrite(my_line, " )= ");
    write(my_line, U);
    writeline(output, my_line);
end print;


entity add32csa is          -- one stage of carry save adder for multiplier
  port(
    b       : in  std_logic;                      -- a multiplier bit
    a       : in  std_logic_vector(31 downto 0);  -- multiplicand
    sum_in  : in  std_logic_vector(31 downto 0);  -- sums from previous stage
    cin     : in  std_logic_vector(31 downto 0);  -- carrys from previous stage
    sum_out : out std_logic_vector(31 downto 0);  -- sums to next stage
    cout    : out std_logic_vector(31 downto 0)); -- carrys to next stage
end add32csa;


ARCHITECTURE circuits of add32csa IS
  SIGNAL zero : STD_LOGIC_VECTOR(31 downto 0) := X"00000000";
  SIGNAL aa : std_logic_vector(31 downto 0) := X"00000000";
  
  COMPONENT fadd    -- duplicates entity port
    PoRT(a    : in  std_logic;
         b    : in  std_logic;
         cin  : in  std_logic;
         s    : out std_logic;
         cout : out std_logic);
  end comPonent fadd;
  
begin  -- circuits of add32csa
  aa <= a when b='1' else zero after 1 ns;
  stage: for I in 0 to 31 generate
    sta: fadd port map(aa(I), sum_in(I), cin(I) , sum_out(I), cout(I));
  end generate stage;  
end architecture circuits; -- of add32csa
