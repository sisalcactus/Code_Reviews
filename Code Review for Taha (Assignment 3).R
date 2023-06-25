#HANGMAN GAME

#Creating a function to check credibility of character inputted by user
#User input can't have multiple characters, non-letter characters, numbers, or already have been guessed before.
check_user_input <- function(user_guess, guessed_letters) {
  if (nchar(user_guess) != 1 || user_guess %in% guessed_letters || !(grepl("^[a-zA-Z]$", user_guess))) {
    print("Invalid input: Please input a single letter that has not been guessed before.")
    return(FALSE)
  }
  return(TRUE)
}

#Creating a function for the visual display given to users so that they know which letters they have guessed in the word, and in which order.
#Created a function called visual_display that uses three parameters, mystery_word, user_guess, word_display.
#First, I split the mystery word into its individual characters using str split, and then find the length of the mystery word
#Then, used a for loop that checks if every letter in the mystery matches the user guess, and if it does, then the word display is updated with the letter in its correct spot.

visual_display <- function(mystery_word, user_guess, word_display) {
  mystery_word <- strsplit(mystery_word, "")[[1]]
  word.length <- length(mystery_word)
  
  for (i in 1:word.length){
    if (mystery_word[i] == user_guess) {
      word_display[i] <- user_guess
    }
  }
  print(word_display)
  return(word_display)
}




play_game <- function() {
  #Prepare a dictionary of words to choose from and save it in a txt file (one column) and save it in the project directory
  wordlist <- readLines("Hangman_Words.txt")
  #Select a random word from the list using the sample() function, and assign it to the variable mystery_word.
  mystery_word <- sample(wordlist, 1, prob = NULL)
  #Set a mistake counter, that will increase whenever the user inputs a wrong letter
  mistake <- 0
  #An empty vector called guessed_letters will be updated everytime the user makes a guess, to ensure that they do not enter the same letter twice.
  guessed_letters <- c()
  #Set a blank word display of "_" that correspond to the number of letters in the mystery word.
  word_display <- noquote(rep('_', nchar(mystery_word), sep = ''))
  
  
  #Display introduction of game to users before they start, as well as instructions. Users are allowed 5 wrong guesses before the game is over.
  print("Welcome to Hangman!")
  print("The objective of the game is to guess the mystery word, one letter at a time")
  print("You are allowed 5 wrong guesses before the game is over. You can also guess the mystery word in full after every turn")
  print("Good luck!")
  print(paste0("The length of the mystery word is ", nchar(mystery_word), " letters long."))
  print(word_display)
  
  
  #The game itself is nested in a repeat loop, starting with a prompt for user entry.
  repeat {
    user_guess <- toupper(readline(prompt = "Please enter a single letter: "))
    
    #The check_user_input()\ function is called to ensure that the user input is valid. If it is not, they are notified and 'next' directs them back to the beginning where they are prompted to enter another letter,
    if (check_user_input(user_guess, guessed_letters) == FALSE) {
      next
      # If the user input is valid, the letter is added to the guessed_letters vector, and the game continues.
    } else {
      guessed_letters <- c(guessed_letters, user_guess)
      
      #Check to see if the user guess is in the mystery word, and if it is, notify the user.
      if (user_guess %in% strsplit(mystery_word, "")[[1]]) {
        print(paste0(user_guess, " is in the mystery word."))
        
        #If it is not in the mystery word, notify the user and add 1 to the mistake counter. The user is notified how many mistakes they have left.
      } else {
        print(paste0(user_guess, " is not in the mystery word."))
        mistake <- mistake + 1
        print(paste0("You have ", 5-mistake, " mistakes left."))
      }
      #If the mistake counter equals 5, inform the user that the game is over and what the mystery word was.
      #'break' ends the repeat loop and the game
      if (mistake >= 5) {
        print("No more remaining guesses. Game Over :(")
        print(paste0("The mystery word was: ", mystery_word))
        break
      }
    }
    #Visual display function is called to show the user of their progress.
    # By assigning the output of the visual_display function to the word_display variable, the word_display variable is updated after every turn with all previoiusly guessed correct answers.
    word_display <- visual_display(mystery_word, user_guess, word_display)
    
    #After every guess, the user is asked if they would like to guess the entire word.
    choice <- toupper(readline(prompt = "Would you like to guess the mystery word? (Y/N): "))
    if (choice == "N") {
      next
    } else {
      guess_the_word <- toupper(readline(prompt = "Enter guess: "))
    }
    
    #If the guess is correct, inform the user that they have won, and end the game.
    if (guess_the_word == mystery_word) {
      print("You guessed the mystery word! Congratulations, you win!")
      break
      #If their guess is wrong, inform the user that the guess is not the mystery word, and how many mistakes they have left.
      #A mistake is not added if a user guess the entire word incorrectly, only whehn they guess a letter incorrectly.
    } else {
      print(paste0("Sorry ", guess_the_word, " is not the mystery word."))
      print(paste0("You have ", 5-mistake, " mistakes left."))
    }
  }
}

play_game()


#' Peer Reviewer Shawn's Comments ####

#' Required Functionality:
  #' Dictionary is correctly prepared (in .txt file and 1 column).
  #' Word list is read correctly
  #' Sampling occurs correctly
  #' Length of word is provided in the prompt
  #' Number of tries is given with clear instructions
  #' User input is correctly requested (great use of toupper() in the line 58 so you didn't have to define a new variable) and the correct error message is printed for non-letter characters exceeding 1 character
  #' Correct notification that user inputs are in the secret word with next letter requested properly
  #' Next letters are correctly requested until the user runs out of all attempts
  #' Game is over with all attempts used up with correct prompt shown and ended
  #' No infinite loop appeared; great job
  #' Correct letters and wrong letters with remaining tries are prompted

#' Bonus Functionality:
  #' Characters are checked whether they are letters or not; elegant use of regex grepl() with the ^ and $; great work
  #' Both upper and lower case letters allowed using grepl(); great job
  #' Visual clue of progress is given with each correct guess using the rep() function with "_" and updates as the game progressed; this helps the user stay informed so great user experience
  #' Clever use of toupper() to standardize inputs
  #' Ingenious use of noquote() and rep() to set the blank tiles (visual clue of progress) with the appropriately placed code to update that clue in line 87
  #' Excellent integration of using option to guess the full word and toupper() to standardize the input

#' Style and Organization:
  #' Very good use of functions and loop-related code (highlights being "next" and "break"), with definitions at the start of the code, to keep the main loop short and sweet
  #' Logical placement of code and streamlined comments appropriately placed throughout
  #' Print messages are short and in separate lines so we avoid walls of text; great job
  #' Clever and concise one-line code in the validity check (line 6)
  #' The first code block (starting with line 5) is particularly concise and works as expected with clever Boolean usage; amazing
  #' The two functions you set up at the start of the code show the relevant parameters; great work
  #' Detailed explanations throughout in your comments; fantastic attention to detail and focus on the purpose of the line of code at hand

#' Recommendations:
  #' Line 19: to set a vector of characters, could use unlist() rather than [[1]] if preferred
  #' Line 34: to streamline the code, we could take out the creation and calling of the play_game() function; instead, we could just have, right after defining the visual_display() function, "wordlist <- readLines("Hangman_Words.txt")" and onward (basically keeping everything the same except taking out lines 34, 108, and 110 to keep your code concise)
  #' Line 38: "prob = NULL" is not needed as that's in the default
  #' Line 44: could place this code in the visual_display() function for organization and replace single quote marks with double quote marks for consistency in style
  #' Line 50: might be a good idea to tell the reader that even if they guess all letters correctly, they have to guess the full word to win the game (please see my comment regarding line 89)
  #' Line 52: update prompt to: "The mystery word is", nchar(mystery_word), "letters long."
  #' Line 68: can replace "strsplit(mystery_word, "")[[1]]" with "mystery_word"
  #' Line 75: might be a good idea to add a space before and after the minus sign for style
  #' Line 79: can use "mistake == 5" instead since it can't get higher than 5 so the ">" will not be possible to reach (so "==" may be more applicable)
  #' Line 87: could place this code into the "if (user_guess %in% strsplit(mystery_word, "")[[1]])" (line 68) for organization)
  #' Line 89: I had guessed all letters of the word correctly (the visual clue of progress is filled) and was still prompted to enter the full word (I selected N for "Would you like to guess the mystery word? (Y/N)"); maybe we could add code to break the loop and say I won without needing to enter the full word
  #' Line 90: the user is prompted to enter Y or N, and I was able to enter non-characters that prompted the message of "Enter guess"; maybe add another validity check here so the user is restricted to entering only Y or N (in line 93; maybe we could add another regex here)
  #' Line 94: might be more intuitive to have "guessed_word" (noun) than "guess_the_word" (imperative)
  #' General:
    #' for concision, can use paste() rather than paste0() (and remove the unnecessary spaces) for lines such as 52, 69, and 81
    #' maybe add a line that prompts the user to play again whether they have won or lost: could do this outside the loop
    #' maybe add a message that gives users on an ever-updating list of wrong letters they've guessed (could create a new vector to which newly made incorrect guesses are added, e.g. something like wrong_guesses <- c() as an initial condition and wrong_guesses <- c(wrong_guesses, guessed_letters) in an if block to update, and shown in a print message)
    #' might be a good idea to notify the user their remaining tries even when they've inputted a correct letter (just another print() message but without 1 being added to "mistakes")
  
#' Final Thoughts:
  #' The code is definitely efficient, readable, easy to follow, and streamlined with robust defence and fluid user experience, overall surpassing the expectations for this assignment.
  #' Outstanding work, Taha!