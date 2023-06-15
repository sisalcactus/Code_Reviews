### ASSIGNEMENT 2 
### Ramiz Khan
### BTC1855H

### Code Reviewer: Shawn Chang (notes at end of script)

# Get user to input value (hopefully numeric and three digits)
user_input <- readline(prompt = "Please enter a three digit number: ")

# Nested if() statement to check:
#   (1) if input is numeric
#   (2) if input is a natural number
#   (3) if input is a three digits
#   (4) if input is an armstrong/narcissistic number

# Checking for (1) using is.numeric() which returns boolean checking if input can be evaluated as a number
if(is.numeric(as.numeric(user_input)) &
   
   # Checking for (2) using mod (%%) which checks for a remainder
   (as.numeric(user_input)) %% 1 == 0 &
   
   # Checking for (3) by using nchar() which can count characters from the input and determine if its equivalent to 3
   (nchar(as.character(user_input))) == 3) {
  
  # Checking for (4) by taking each number from the input, cubing it and summing them to determine if it matches the original input
  if((as.numeric(substr(user_input,1,1))^3 + as.numeric(substr(user_input,2,2))^3 + as.numeric(substr(user_input,3,3))^3) == as.numeric(user_input)) {
    
    # Output if (1),(2), and (3) are true but (4) is not    
    print("The number you have inputted is a narcissistic/armstrong number!")
  } else {
    
    # Output if all checks are true
    print("The number you have inputted is NOT a narcissistic/armstrong number, please try again.")
  }
} else {
  if (is.na(as.numeric(user_input))) {
    
    # Output if (1) is false
    print("Error: Non-Numeric value entered, program will be terminated")
  } else {
    
    # Output if (2) and/or (3) is false
    print("Error: Numeric value is non-natural or not exactly three digits, program will be terminated")
  }
}

# *Comments from Code Reviewer (Shawn)*
  # 1. Overall concise and elegant code with clear, explanatory comments. Please find suggestions below:
  # 2. Line 8: I suggest keeping the variable name ("user_input") more concise to conserve space, especially as later you have a relatively long line of code (the cubing and summing calculation)
  # 3. Lines 10-14: I appreciate the organization. Great work.
  # 4. Line 12: A minor correction to the comment "(2) if input is a natural number": We should have "...is an integer"
        # Rationale: the mod function checks for integers including negative ones, which means not just natural numbers
  # 5. Line 19: I think the priority being checked for is whether the input is an integer; maybe revisit the wording
  # 6. Line 23: Good work with using nchar(); this wasn't explicitly asked for so you went above and beyond with this one
  # 7. Line 24:  It might be interesting to add a line of code to check for positive numbers (as.numeric(user_input)>0) 
  # 8. Line 25: To avoid ambiguity, the wording in the comment can be tweaked to "each *digit* from the input" as the input itself itself is number
  # 9. Line 26: I like the code for its elegance and simplicity. Well done, Ramiz. If you were interested, another way to find the three numbers are to use %% (to extract the rightmost digit), floor() (to extract the leftmost digit), and those two extracted digits to extract the middle digit.
      # Although a bit long, it can be a fun brainteaser (an unconventional approach)
          # third_digit <- value %% 10
          # first_digit <- floor(value/100)
          # second_digit <- (value - third_digit - first_digit*100)/10
  # 10. Line 26: It seems that there is a bug with the condition in the if statement (none of the error messages you created appear) if we input "-11" or other negative two-digit numbers.
      # To resolve this, you can add the test for positive numbers (please see my 7th comment);
      # the reason this happens is because right now the nchar() function sees "-11" as a 3 digit number and the other validity checks don't catch it. In other words, numbers like -11 will all be deemed "valid," ultimately being subjected to calculation. This then yields NAs but does not prompt any error messages.
  # 11. As a thought, I suggest capitalizing "armstrong" and "boolean" in the comments (just for accuracy/grammar)



