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

# *Comments from Code Reviewer (Shawn)* ####
  # 1. Overall concise and elegant code with clear, explanatory comments. Please find suggestions below:
  # 2. Line 8: I suggest keeping the variable name ("user_input") more concise to conserve space, especially as later you have a relatively long line of code (the cubing and summing calculation)
  # 3. Lines 10-14: I appreciate the organization. Great work.
  # 4. Line 12: A minor correction to the comment "(2) if input is a natural number": We should have "...is an integer"
        # Rationale: the mod function checks for integers including negative ones, which means not just natural numbers
  # 5. Lines 17 and 20: These look correct; great work with the validity check design
  # 6. Line 19: I think the priority being checked for is whether the input is an integer; maybe revisit the wording
  # 7. Line 23: Good work with using nchar(); this wasn't explicitly asked for so you went above and beyond with this one
  # 8. Line 24:  It might be interesting to add a line of code to check for positive numbers (as.numeric(user_input) > 0)
  # 9. Line 25: To avoid ambiguity, the wording in the comment can be tweaked to "each *digit* from the input" as the input itself itself is number
  # 10. Line 26: I like the code for its elegance and simplicity. Well done, Ramiz. If you were interested, another way to find the three numbers are to use %% (to extract the rightmost digit), floor() (to extract the leftmost digit), and those two extracted digits to extract the middle digit.
      # Rationale: Although a bit long, it can be a fun brainteaser (an unconventional approach)
          # third_digit <- user_input %% 10
          # first_digit <- floor(user_input/100)
          # second_digit <- (user_input - third_digit - first_digit*100)/10
  # 11. Line 26: It seems that there is a bug with the condition in the if statement (none of the error messages you created appear) if we input a negative two-digit number (say, -10).
      # The reason this happens is because right now none of the validity check seem to catch it: -10 passes checks for (1) and (2) as it is an integer; it also passes the check for (3) as the nchar() function sees it as a 3-digit character.
      # In other words, numbers like -10 will all be deemed "valid" according to your checks (1), (2), and (3), ultimately permitting the input to end up being calculated.
      # This then yields NAs in the calculation, with no error messages (that you created) being printed.
      # To resolve this bug, you can add the test for positive numbers (please see my 7th comment);
  # 12. Line 26: Another bug I found was that inputting 000 also prompts the message that I inputted a narcissistic number so maybe we could add a line of code to resolve this
  # 13. Lines 28 and 32: I believe the comments should be switched
  # 14. Lines 36 and onward: I appreciate the distinction between (1) being false and (2)/(3) being false, prompting different error messages.
      # This shows you once again went above and beyond
  # 15. As a thought, I suggest capitalizing "armstrong" and "boolean" in the comments (just for accuracy/grammar)
  # 16. Another thought is that maybe we can adjust the indents so the components of the nested ifs and elses are more clearly shown in their respective if-else blocks
  # 17. Maybe also specify which style guide you used so the code reviewer can use it to cross-reference
  # 18. Collectively, the code is easy to follow and understand and generally works as expected (with the small exception of inputting negative 2-digit numbers and 000 as flagged in my 11th and 12th comments)
      # and the correct messages are printed, appropriately corresponding to the input (I tested inputs like 111, 1000, 0, -11111, aahhh, 1.1, and all 3-digit narcissistic numbers);
      # great work! You did it!
