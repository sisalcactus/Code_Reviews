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
