
# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinyjs) # For additional JavaScript functionality
library(DT)


# --- Game Descriptions ---
gameDescriptions <- list(
  trivia = "Test your knowledge of famous brands, slogans, and advertising campaigns. Answer multiple-choice questions and try to get the highest score!",
  puzzle = "Piece together jumbled brand logos or taglines. Drag and drop the tiles into the correct order to reveal the complete image. (Note: Full drag & drop functionality requires additional setup).",
  clickrace = "React quickly! Click on the branded items as they fall down the screen before they disappear. Faster clicks and accuracy earn more points.",
  match = "Sharpen your memory! Flip over cards to find matching pairs of brands and their associated products. Clear the board in the fewest attempts.",
  tycoon = "Build your own retail empire! Buy inventory (coffee, snacks, tech) from real-world inspired brands, manage your money, attract customers through advertising, and maximize your profits day by day."
)

# --- UI Definition ---
ui <- dashboardPage(
  dashboardHeader(title = "🎮 AdGameHub"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Trivia Challenge", tabName = "trivia", icon = icon("question")),
      menuItem("Brand Puzzle", tabName = "puzzle", icon = icon("puzzle-piece")),
      menuItem("Ad Click Race", tabName = "clickrace", icon = icon("bolt")),
      menuItem("Product Match", tabName = "match", icon = icon("clone")),
      menuItem("Mini Tycoon Sim", tabName = "tycoon", icon = icon("store"))
    )
  ),
  dashboardBody(
    useShinyjs(), # Initialize shinyjs
    tags$head(
      tags$style(HTML("
        body {
          background-color: #f8f9fa;
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        /* Style for the centering container */
        .centered-content {
          width: 80%;
          max-width: 800px;
          margin: 20px auto;
          padding-bottom: 30px;
          /* Ensure animations are visible */
          opacity: 0; /* Start hidden for fade-in */
        }
        .centered-content.visible {
          opacity: 1;
          animation: fadeInUp 0.6s ease-out forwards;
        }
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(20px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        .box {
          border-radius: 12px;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
          padding: 20px;
          margin-bottom: 20px;
        }
        /* Common button styling */
        .game-btn {
          margin: 5px;
          padding: 10px 20px;
          font-size: 15px;
          border-radius: 8px;
          color: white;
          border: none;
          transition: all 0.3s;
          cursor: pointer;
        }
        .game-btn:disabled {
           cursor: not-allowed;
           opacity: 0.6;
        }
        .btn-trivia {
          background-color: #0077b6;
        }
        .btn-trivia:hover:not(:disabled) {
          background-color: #0096c7;
        }
        .btn-puzzle {
          background-color: #e76f51;
        }
        .btn-puzzle:hover:not(:disabled) {
          background-color: #f4a261;
        }
        .btn-race {
          background-color: #d62828;
        }
        .btn-race:hover:not(:disabled) {
          background-color: #e63946;
        }
        .game-heading {
          font-size: 24px;
          font-weight: bold;
          margin-bottom: 15px;
          text-align: center;
        }
        /* Trivia specific styling */
        .trivia-box h3 {
          margin-bottom: 20px;
          text-align: center;
          color: #333;
          min-height: 50px; /* Space for question text */
        }
        .trivia-box .btn-container {
          text-align: center;
          margin-bottom: 15px;
          min-height: 50px; /* Space for buttons */
        }
        .trivia-box h4 {
          text-align: center;
          margin-top: 25px;
          color: #555;
        }
        #overallScoreDisplay, #result { /* Updated ID for score display */
          text-align: center;
          font-size: 18px;
          margin-top: 20px;
          font-weight: bold;
          min-height: 25px; /* Prevent layout jumps */
        }
        #overallScoreDisplay {
          font-size: 20px;
          color: #198754;
        }
        /* Puzzle game styling (Placeholder - Needs JS Library like SortableJS) */
        .puzzle-grid {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          grid-gap: 10px;
          margin: 20px auto;
          max-width: 400px;
          border: 1px dashed #ccc;
          padding: 10px;
          min-height: 300px; /* Placeholder height */
          background-color: #eee;
        }
        .puzzle-piece {
          height: 100px;
          background-color: #f8f9fa;
          border: 2px solid #999;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 24px;
          cursor: move; /* Indicates draggable */
          user-select: none;
          transition: transform 0.2s;
        }
        .puzzle-piece:hover {
          transform: scale(1.05);
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        /* Click race styling */
        #game-area {
          height: 400px;
          background-color: #f0f8ff;
          position: relative;
          overflow: hidden;
          border-radius: 8px;
          border: 2px solid #ddd;
          margin: 20px auto;
        }
        .falling-item {
          position: absolute;
          width: 60px;
          height: 60px;
          background-color: #333; /* Default background */
          color: white;
          display: flex;
          align-items: center;
          justify-content: center;
          border-radius: 50%;
          cursor: pointer;
          user-select: none;
          font-weight: bold;
          font-size: 12px; /* Adjust size */
          text-align: center;
          line-height: 1.2; /* Improve text wrapping */
          padding: 5px;
          box-sizing: border-box;
        }
        /* Example item colors (Add more as needed) */
        .item-nike { background-color: #f60; }
        .item-apple { background-color: #aaa; }
        .item-cocacola { background-color: #e61d2b; }
        .item-samsung { background-color: #1428a0; }
        .item-toyota { background-color: #eb0a1e; }
        .item-adidas { background-color: #000; }

        /* Memory match styling */
        .memory-grid {
          display: grid;
          grid-template-columns: repeat(4, 1fr); /* 4 columns for 12 cards */
          grid-gap: 10px;
          margin: 20px auto;
          max-width: 450px;
        }
        .memory-card {
          height: 100px;
          background-color: #3a86ff; /* Will be covered by front face */
          border-radius: 5px;
          cursor: pointer;
          user-select: none;
          transition: transform 0.4s;
          transform-style: preserve-3d; /* Needed for flip effect */
          position: relative;
        }
        .memory-card .card-face {
          position: absolute;
          width: 100%;
          height: 100%;
          backface-visibility: hidden; /* Hide back when facing front */
          display: flex;
          align-items: center;
          justify-content: center;
          border-radius: 5px;
        }
        .memory-card .card-front {
          background-color: #3a86ff;
          color: #3a86ff; /* Make text same color as bg to hide */
          font-size: 1px;
        }
        .memory-card .card-back {
           background-color: #fff;
           color: #333;
           transform: rotateY(180deg);
           font-size: 14px; /* Show text on back */
           border: 1px solid #ddd;
           text-align: center;
           padding: 5px;
           box-sizing: border-box;
           line-height: 1.2;
        }
        .memory-card.flipped {
          transform: rotateY(180deg);
        }
        .memory-card.matched {
          transform: rotateY(180deg); /* Keep it flipped */
          cursor: default;
          opacity: 0.7; /* Indicate matched */
        }
        .memory-card.matched .card-back {
           background-color: #a7c957; /* Green background for matched */
           color: white;
           border-color: #8aa04b;
        }
        /* Tycoon game styling */
        .tycoon-controls {
          display: flex;
          flex-wrap: wrap;
          justify-content: center;
          gap: 10px;
          margin: 15px 0;
        }
        .tycoon-store {
          background-color: #f8f9fa;
          border-radius: 8px;
          padding: 15px;
          margin-top: 20px;
          border: 2px dashed #ccc;
          min-height: 150px; /* Adjusted height */
          position: relative;
        }
        .store-item {
          display: inline-block;
          margin: 5px;
          padding: 8px 12px;
          background-color: #e9ecef;
          border-radius: 5px;
          font-size: 14px;
        }
        .tycoon-stats {
          display: flex;
          flex-wrap: wrap; /* Allow wrapping on small screens */
          justify-content: space-around; /* Better spacing */
          background-color: #fff;
          padding: 10px 15px;
          border-radius: 8px;
          margin-bottom: 15px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          gap: 15px; /* Space between stats */
        }
         /* Feedback styling */
        .feedback {
           margin-top: 15px;
           font-weight: bold;
           min-height: 40px; /* Prevent layout jumps */
        }
        .alert { /* Basic alert styling */
            padding: 10px 15px;
            margin-bottom: 10px;
            border: 1px solid transparent;
            border-radius: 4px;
        }
        .alert-success {
            color: #155724;
            background-color: #d4edda;
            border-color: #c3e6cb;
        }
        .alert-danger {
            color: #721c24;
            background-color: #f8d7da;
            border-color: #f5c6cb;
        }
        .alert-info {
            color: #0c5460;
            background-color: #d1ecf1;
            border-color: #bee5eb;
        }
        .alert-warning {
            color: #856404;
            background-color: #fff3cd;
            border-color: #ffeeba;
        }
      "))
    ),
    tabItems(
      # --- Trivia Tab ---
      tabItem(tabName = "trivia",
              div(class = "centered-content", # Add ID for animation targeting
                  id = "content_trivia",
                  box(
                    title = NULL,
                    width = 12, status = "info", solidHeader = FALSE,
                    tags$div(
                      class = "trivia-box",
                      tags$h3(textOutput("triviaQuestion", inline = TRUE)),
                      tags$div(class = "btn-container",
                               uiOutput("triviaOptions") # Dynamic buttons
                      ),
                      uiOutput("result", class="feedback"), # Use uiOutput for HTML feedback
                      tags$h4("Overall Score:"), # Changed label slightly
                      textOutput("overallScoreDisplay") # Changed ID for clarity
                    ),
                    actionButton("nextQuestion", "Next Question", class = "game-btn btn-trivia",
                                 style = "display: none; margin: 15px auto; display: block;") # Center button
                  )
              )
      ),
      
      # --- Puzzle Tab ---
      tabItem(tabName = "puzzle",
              div(class = "centered-content", id = "content_puzzle",
                  box(
                    title = NULL, # Use game heading instead
                    width = 12, status = "warning", solidHeader = FALSE,
                    tags$p(class="game-heading", "🧩 Brand Puzzle"),
                    tags$p("Drag and drop the puzzle pieces to recreate the brand logo. (Note: Drag & Drop needs a JavaScript library like SortableJS for full functionality - this is a placeholder)."),
                    
                    # Puzzle controls
                    div(class = "text-center",
                        actionButton("newPuzzle", "New Puzzle", class = "game-btn btn-puzzle"),
                        actionButton("checkPuzzle", "Check Solution", class = "game-btn btn-puzzle")
                    ),
                    
                    # Puzzle grid
                    uiOutput("puzzleGrid", class = "puzzle-grid"), # Use uiOutput
                    
                    # Puzzle feedback
                    div(id = "puzzleFeedback", class = "feedback text-center")
                  )
              )
      ),
      
      # --- Click Race Tab ---
      tabItem(tabName = "clickrace",
              div(class = "centered-content", id = "content_clickrace",
                  box(
                    title = NULL,
                    width = 12, status = "danger", solidHeader = FALSE,
                    tags$p(class="game-heading", "⚡ Ad Click Race"),
                    tags$p("Click on falling brand items before they disappear!"),
                    
                    # Game controls & Stats
                    div(class = "text-center", style="margin-bottom: 15px;",
                        actionButton("startRace", "Start Game", class = "game-btn btn-race"),
                        span(style = "margin-left: 15px; font-weight: bold;",
                             "Time: ", textOutput("raceTimer", inline = TRUE), "s"),
                        span(style = "margin-left: 15px; font-weight: bold;",
                             "Score: ", textOutput("raceScore", inline = TRUE))
                    ),
                    
                    # Game area
                    div(id = "game-area"),
                    
                    # Game feedback
                    div(id = "raceFeedback", class = "feedback text-center")
                  )
              )
      ),
      
      # --- Product Match Tab ---
      tabItem(tabName = "match",
              div(class = "centered-content", id = "content_match",
                  box(
                    title = NULL,
                    width = 12, status = "primary", solidHeader = FALSE,
                    tags$p(class="game-heading", "🔄 Product Match"),
                    tags$p("Find matching pairs of products and brands in this memory game."),
                    
                    # Game controls & Stats
                    div(class = "text-center", style="margin-bottom: 15px;",
                        actionButton("newMatchGame", "New Game", class = "game-btn",
                                     style = "background-color: #4361ee; color: white;"),
                        span(style = "margin-left: 15px; font-weight: bold;",
                             "Attempts: ", textOutput("matchAttempts", inline = TRUE)),
                        span(style = "margin-left: 15px; font-weight: bold;",
                             "Matches: ", textOutput("matchesMade", inline = TRUE))
                    ),
                    
                    # Memory card grid
                    uiOutput("memoryGrid", class = "memory-grid"), # Use uiOutput
                    
                    # Game feedback
                    div(id = "matchFeedback", class = "feedback text-center")
                  )
              )
      ),
      
      # --- Tycoon Sim Tab ---
      tabItem(tabName = "tycoon",
              div(class = "centered-content", id = "content_tycoon",
                  box(
                    title = NULL,
                    width = 12, status = "success", solidHeader = FALSE,
                    tags$p(class="game-heading", "🏗️ Mini Tycoon Sim"),
                    tags$p("Build your store empire by purchasing and managing branded products."),
                    
                    # Tycoon stats
                    div(class = "tycoon-stats",
                        div(span("Day: ", strong(textOutput("day", inline = TRUE)))),
                        div(span("Money: $", strong(textOutput("money", inline = TRUE)))),
                        div(span("Customers: ", strong(textOutput("customers", inline = TRUE)))),
                        div(span("Reputation: ", strong(textOutput("reputation", inline = TRUE)), "/100"))
                    ),
                    
                    # Tycoon controls
                    div(class = "tycoon-controls",
                        actionButton("buyCoffee", "Buy Coffee ($100)", class = "game-btn", style = "background-color: #6f4e37;"), # Brown for coffee
                        actionButton("buySnack", "Buy Snacks ($80)", class = "game-btn", style = "background-color: #ffc300; color: #333;"), # Yellow for snacks
                        actionButton("buyTech", "Buy Tech ($200)", class = "game-btn", style = "background-color: #555;"), # Grey for tech
                        actionButton("advertise", "Advertise ($50)", class = "game-btn", style = "background-color: #f72585;"), # Pink for advertise
                        actionButton("nextDay", "Next Day", class = "game-btn", style = "background-color: #70e000;") # Green for next day
                    ),
                    
                    # Store display
                    tags$h4("Your Store:", style = "margin-top: 20px; text-align: center;"),
                    div(class = "tycoon-store", id = "storeDisplay"),
                    
                    # Game feedback
                    div(id = "tycoonFeedback", class = "feedback")
                  )
              )
      )
    ) # End tabItems
  ) # End dashboardBody
) # End dashboardPage

# --- Server Logic ---
server <- function(input, output, session) {
  
  # ---- Global Game State ----
  gameScores <- reactiveValues(trivia = 0, puzzle = 0, race = 0, match = 0, tycoon = 0)
  # Flag to ensure startup code runs only once
  hasInitialized <- reactiveVal(FALSE)
  
  # Overall score calculation and display
  output$overallScoreDisplay <- renderText({ # Matched ID in UI
    scores_list <- isolate(reactiveValuesToList(gameScores))
    # Ensure all elements are numeric before summing
    numeric_scores <- sapply(scores_list, function(x) if(is.numeric(x)) x else 0)
    sum(unlist(numeric_scores), na.rm = TRUE) # CORRECTED SUM LOGIC
  })
  
  
  # ---- Helper Functions ----
  # Function to add animation class
  triggerAnimation <- function(selector) {
    removeClass(selector, "visible") # Remove first to allow re-triggering
    delay(10, { # Use a tiny delay
      addClass(selector, "visible")
    })
  }
  
  # ---- TRIVIA GAME ----
  # ... (Trivia questions list remains the same) ...
  triviaQuestions <- list(
    list(q = "Which company owns the brand 'Tide'?", o = c("Unilever", "Procter & Gamble", "Nestlé"), a = 2),
    list(q = "Which of these is NOT a Nike slogan?", o = c("Just Do It", "Impossible Is Nothing", "Find Your Greatness"), a = 2),
    list(q = "Which fast food chain uses 'I'm Lovin' It'?", o = c("McDonald's", "Burger King", "Wendy's"), a = 1),
    list(q = "Which company makes 'Surface' computers?", o = c("Apple", "Microsoft", "Dell"), a = 2),
    list(q = "Which brand has red-soled shoes?", o = c("Jimmy Choo", "Christian Louboutin", "Manolo Blahnik"), a = 2),
    list(q = "What does BMW stand for?", o = c("Bavarian Motor Works", "British Motor Works", "Berlin Motor Works"), a = 1),
    list(q = "'Think Different' was a slogan for which company?", o = c("IBM", "Microsoft", "Apple"), a = 3)
  )
  
  currentQuestionData <- reactiveVal(NULL) # Start NULL initially
  triviaAnswered <- reactiveVal(FALSE)
  
  loadTriviaQuestion <- function() { # ... (loadTriviaQuestion function remains the same) ...
    q_idx <- sample(1:length(triviaQuestions), 1)
    currentQuestionData(triviaQuestions[[q_idx]])
    triviaAnswered(FALSE)
    hide("nextQuestion")
    output$result <- renderUI({""})
    lapply(1:3, function(i) enable(paste0("option", i)))
  }
  
  output$triviaQuestion <- renderText({ # ... (renderText remains the same) ...
    req(currentQuestionData())
    currentQuestionData()$q
  })
  
  output$triviaOptions <- renderUI({ # ... (renderUI remains the same) ...
    req(currentQuestionData())
    qData <- currentQuestionData()
    buttons <- lapply(1:length(qData$o), function(i) {
      actionButton(inputId = paste0("option", i), label = qData$o[i], class = "game-btn btn-trivia")
    })
    if (triviaAnswered()) {
      buttons <- lapply(buttons, function(btn) { shinyjs::disable(btn$attribs$id); btn })
    }
    do.call(tagList, buttons)
  })
  
  lapply(1:3, function(i) { # ... (answer observers remain the same) ...
    observeEvent(input[[paste0("option", i)]], {
      req(currentQuestionData())
      if (!triviaAnswered()) {
        qData <- currentQuestionData()
        correctAnswerIndex <- qData$a
        correctAnswerText <- qData$o[correctAnswerIndex]
        if (i == correctAnswerIndex) {
          output$result <- renderUI({tags$div(class="alert alert-success", "✅ Correct!")})
          gameScores$trivia <- gameScores$trivia + 1
        } else {
          output$result <- renderUI({tags$div(class="alert alert-danger",
                                              paste0("❌ Incorrect! The answer is: ", correctAnswerText))})
        }
        triviaAnswered(TRUE)
        show("nextQuestion")
        lapply(1:length(qData$o), function(idx) disable(paste0("option", idx)))
      }
    })
  })
  
  observeEvent(input$nextQuestion, { # ... (next question observer remains the same) ...
    loadTriviaQuestion()
  })
  
  
  # ---- PUZZLE GAME ----
  # ... (Puzzle logic remains the same placeholder) ...
  puzzleBrands <- list(
    list(name = "Coca-Cola", pieces = c("CO", "CA", "CO", "LA", "®", "™", "EST", "1886", "®")),
    list(name = "Apple", pieces = c("AP", "PL", "E", "", "Think", "Diff", "er", "ent", "®")),
    list(name = "Nike", pieces = c("NI", "KE", "✓", "Just", "Do", "It", "®", "™", "®"))
  )
  currentPuzzleData <- reactiveVal(NULL)
  observeEvent(input$newPuzzle, {
    puzzleIdx <- sample(1:length(puzzleBrands), 1)
    brandData <- puzzleBrands[[puzzleIdx]]
    shuffledPieces <- sample(brandData$pieces)
    currentPuzzleData(list(name = brandData$name, solution = brandData$pieces, current = shuffledPieces))
    pieceHTML <- lapply(1:length(shuffledPieces), function(i) {
      tags$div(class = "puzzle-piece", id = paste0("piece", i), shuffledPieces[i])
    })
    output$puzzleGrid <- renderUI({ do.call(tagList, pieceHTML) })
    html("puzzleFeedback", "")
  })
  observeEvent(input$checkPuzzle, {
    if (runif(1) > 0.5) {
      html("puzzleFeedback", "<div class='alert alert-success'>Congratulations! (Placeholder Check)!</div>")
      gameScores$puzzle <- gameScores$puzzle + 3
    } else {
      html("puzzleFeedback", "<div class='alert alert-warning'>Not quite right (Placeholder Check).</div>")
    }
  })
  
  # ---- CLICK RACE GAME ----
  # ... (Click race logic remains the same) ...
  raceState <- reactiveValues(active = FALSE, timer = 30, score = 0)
  timer_invalidate <- reactiveVal(0)
  timer_running <- reactiveVal(FALSE)
  observeEvent(input$startRace, {
    if (!raceState$active) {
      raceState$active <- TRUE
      timer_running(TRUE)
      raceState$timer <- 30
      raceState$score <- 0
      html("game-area", "")
      html("raceFeedback", "")
      disable("startRace")
      timer_invalidate(runif(1))
    }
  })
  observe({
    timer_invalidate()
    if(timer_running()){
      isolate({
        if (raceState$timer > 0) {
          raceState$timer <- raceState$timer - 1
          if (runif(1) < 0.6) { spawnItem() }
          later::later(function() timer_invalidate(runif(1)), 1)
        } else {
          raceState$active <- FALSE
          timer_running(FALSE)
          html("raceFeedback", paste0("<div class='alert alert-info'>Game over! Final score: ", isolate(raceState$score), "</div>"))
          enable("startRace")
          gameScores$race <- gameScores$race + isolate(raceState$score)
        }
      })
    }
  })
  spawnItem <- function() { # ... (spawnItem function remains the same) ...
    itemId <- paste0("item", sample(100000:999999, 1))
    brands <- c("Nike", "Apple", "Coca-Cola", "Samsung", "Toyota", "Adidas")
    brand <- sample(brands, 1)
    left <- runif(1) * 85
    duration <- sample(2500:4500, 1)
    item_color_class <- paste0("item-", tolower(gsub("[^a-zA-Z0-9]", "", brand)))
    itemHTML <- paste0("<div class='falling-item ", item_color_class, "' id='", itemId, "' style='left:", left, "%; top: -60px;'>", brand, "</div>")
    runjs(paste0("$('#game-area').append(\"", itemHTML, "\");"))
    click_js <- paste0("
       $('#", itemId, "').on('click', function() {
         $(this).remove();
         Shiny.setInputValue('itemClicked', {id: '", itemId, "', ts: Date.now()}, {priority: 'event'});
       });")
    runjs(click_js)
    animate_js <- paste0("
       $('#", itemId, "').animate({top: '100%'},
       { duration: ", duration, ", easing: 'linear', complete: function() { $(this).remove(); } });")
    runjs(animate_js)
  }
  observeEvent(input$itemClicked, { # ... (itemClicked observer remains the same) ...
    if(raceState$active) { raceState$score <- raceState$score + 1 }
  })
  output$raceTimer <- renderText({ raceState$timer })
  output$raceScore <- renderText({ raceState$score })
  
  # --- Within the server function ---
  
  # ---- MEMORY MATCH GAME ----
  matchState <- reactiveValues(
    cards = list(),      # Holds current card text values
    values = list(),     # Holds the underlying matching value for each card index
    indices = list(),    # Original indices before shuffling (maps cardId back to original value/text)
    flipped_ids = c(),   # IDs of currently flipped cards (max 2)
    matched_ids = c(),   # IDs of cards that have been successfully matched
    matchesMade = 0,
    attempts = 0,
    locked = FALSE       # Prevent clicks during check/animation
  )
  
  # Brand-product pairs for memory game
  allPairs <- list(
    list(b = "Nike", p = "Air Max"), list(b = "Apple", p = "iPhone"),
    list(b = "Coca-Cola", p = "Diet Coke"), list(b = "Samsung", p = "Galaxy"),
    list(b = "Toyota", p = "Corolla"), list(b = "Adidas", p = "Superstar"),
    list(b = "Microsoft", p = "Surface"), list(b = "Pepsi", p = "Mountain Dew")
    # Add more pairs if needed (grid size needs adjustment)
  )
  num_pairs_to_use <- 6 # Needs 12 cards (4x3 grid or adjust grid CSS)
  
  # Start a new memory match game
  observeEvent(input$newMatchGame, {
    # Reset game state
    matchState$flipped_ids <- c()
    matchState$matched_ids <- c()
    matchState$matchesMade <- 0
    matchState$attempts <- 0
    matchState$locked <- FALSE
    html("matchFeedback", "")
    
    selectedPairs <- sample(allPairs, num_pairs_to_use)
    
    cards_text <- c()
    cards_values <- c()
    for (i in 1:length(selectedPairs)) {
      cards_text <- c(cards_text, selectedPairs[[i]]$b, selectedPairs[[i]]$p)
      cards_values <- c(cards_values, i, i) # Assign same value to matching pairs
    }
    
    # Store original indices before shuffling
    original_indices <- 1:length(cards_text)
    shuffleIdx <- sample(original_indices)
    
    matchState$cards <- cards_text[shuffleIdx]
    matchState$values <- cards_values[shuffleIdx]
    # Create a map from the shuffled position back to the original value index
    # We don't strictly need this if we store values alongside cards after shuffling,
    # but it shows the shuffled mapping if needed later.
    
    # Generate card HTML
    cardHTML <- lapply(1:length(matchState$cards), function(i) {
      cardId <- paste0("card", i) # ID corresponds to the *shuffled* position
      tags$div(
        class = "memory-card",
        id = cardId,
        # Pass ID and a timestamp to ensure Shiny sees distinct events
        onclick = paste0("Shiny.setInputValue('cardClicked', {id: '", cardId, "', ts: Date.now()}, {priority: 'event'})"),
        # Card faces for flip animation
        tags$div(class="card-face card-front"),
        tags$div(class="card-face card-back", matchState$cards[i]) # Text comes from shuffled list
      )
    })
    
    output$memoryGrid <- renderUI({ do.call(tagList, cardHTML) })
  })
  
  
  # Handle card clicks
  observeEvent(input$cardClicked, {
    req(input$cardClicked) # Ensure click data is present
    cardId <- input$cardClicked$id # Get ID from the list sent by JS
    
    # ---- Pre-computation ----
    # Get the index corresponding to the card ID (1-based)
    cardIndex <- as.numeric(sub("card", "", cardId))
    
    # ---- Input Validation and State Checks ----
    # Ignore click if:
    # 1. Board is locked
    # 2. Less than 2 cards already flipped AND this card is already flipped
    # 3. This card is already matched
    # 4. Card index is invalid (shouldn't happen if UI is correct)
    if (matchState$locked ||
        (length(matchState$flipped_ids) < 2 && cardId %in% matchState$flipped_ids) ||
        cardId %in% matchState$matched_ids ||
        cardIndex <= 0 || cardIndex > length(matchState$values) ) {
      return()
    }
    
    # ---- Flip Card Visually and Update State ----
    addClass(cardId, "flipped")
    matchState$flipped_ids <- c(matchState$flipped_ids, cardId)
    
    # ---- Logic for 2 Cards Flipped ----
    if (length(matchState$flipped_ids) == 2) {
      matchState$locked <- TRUE # Lock board during check
      matchState$attempts <- matchState$attempts + 1
      
      # Get IDs and indices of the two flipped cards
      card1_id <- matchState$flipped_ids[1]
      card2_id <- matchState$flipped_ids[2]
      card1_index <- as.numeric(sub("card", "", card1_id))
      card2_index <- as.numeric(sub("card", "", card2_id))
      
      # Get the underlying values from the server state based on index
      value1 <- matchState$values[card1_index]
      value2 <- matchState$values[card2_index]
      
      # ---- Check for Match ----
      if (value1 == value2) {
        # --- MATCH FOUND ---
        # Mark as matched internally
        matchState$matched_ids <- c(matchState$matched_ids, card1_id, card2_id)
        # Add visual matched class
        addClass(card1_id, "matched")
        addClass(card2_id, "matched")
        
        matchState$matchesMade <- matchState$matchesMade + 1
        gameScores$match <- gameScores$match + 2 # Add score
        
        html("matchFeedback", "<div class='alert alert-success'>Match found!</div>")
        delay(800, html("matchFeedback", "")) # Clear feedback after short delay
        
        # Clear the flipped state, board remains locked until feedback cleared
        matchState$flipped_ids <- c()
        matchState$locked <- FALSE # Unlock board after processing match
        
        # Check for game completion
        if (matchState$matchesMade >= num_pairs_to_use) {
          html("matchFeedback", paste0("<div class='alert alert-success'>
                                             Game complete! You found all matches in ",
                                       isolate(matchState$attempts), " attempts.</div>"))
        }
        
      } else {
        # --- NO MATCH ---
        html("matchFeedback", "<div class='alert alert-danger'>No match. Try again!</div>")
        
        # Store the IDs to flip back *before* the delay starts
        ids_to_flip_back <- matchState$flipped_ids
        # Clear the internal flipped state immediately
        matchState$flipped_ids <- c()
        
        # Flip back visually after a delay
        delay(1200, {
          # Use the stored IDs
          removeClass(ids_to_flip_back[1], "flipped")
          removeClass(ids_to_flip_back[2], "flipped")
          matchState$locked <- FALSE # Unlock board AFTER flip back animation time
          html("matchFeedback", "") # Clear feedback AFTER flip back
        })
      } # End if/else (match check)
    } # End if 2 cards flipped
  }) # End observeEvent(input$cardClicked)
  
  # Update display of matches and attempts
  output$matchesMade <- renderText({ matchState$matchesMade })
  output$matchAttempts <- renderText({ matchState$attempts })

  
  # ---- TYCOON GAME ----
  # ... (Tycoon logic remains the same) ...
  tycoonState <- reactiveValues(money = 500, customers = 0, reputation = 10, storeItems = list(), day = 1)
  output$money <- renderText({ tycoonState$money })
  output$customers <- renderText({ tycoonState$customers })
  output$reputation <- renderText({ tycoonState$reputation })
  output$day <- renderText({ tycoonState$day })
  buyItem <- function(cost, itemGenerator, customerRange, feedbackId, typeName) { # ... (buyItem function remains the same) ...
    if (tycoonState$money >= cost) {
      tycoonState$money <- tycoonState$money - cost; newItem <- itemGenerator()
      tycoonState$storeItems <- c(tycoonState$storeItems, newItem); updateStoreDisplay()
      tycoonState$customers <- tycoonState$customers + sample(customerRange, 1)
      html(feedbackId, paste0("<div class='alert alert-success'>Purchased ", newItem, "!</div>")); delay(3000, html(feedbackId, ""))
    } else { html(feedbackId, paste0("<div class='alert alert-danger'>Not enough money for ", typeName, "!</div>")); delay(3000, html(feedbackId, "")) } }
  generateCoffee <- function() { types <- c("Arabica", "Espresso", "Latte", "Mocha", "Cappuccino"); brands <- c("Starbucks", "Nespresso", "Dunkin'", "Peet's", "Folgers"); paste0(sample(brands, 1), " ", sample(types, 1)) }
  generateSnack <- function() { types <- c("Chips", "Cookies", "Pretzels", "Nuts", "Chocolate"); brands <- c("Lay's", "Oreo", "Snyder's", "Planters", "Hershey's"); paste0(sample(brands, 1), " ", sample(types, 1)) }
  generateTech <- function() { types <- c("Smartphone", "Tablet", "Laptop", "Headphones", "Speaker"); brands <- c("Apple", "Samsung", "Sony", "Bose", "Microsoft"); paste0(sample(brands, 1), " ", sample(types, 1)) }
  observeEvent(input$buyCoffee, { buyItem(100, generateCoffee, 2:5, "tycoonFeedback", "coffee") })
  observeEvent(input$buySnack, { buyItem(80, generateSnack, 1:4, "tycoonFeedback", "snacks") })
  observeEvent(input$buyTech, { buyItem(200, generateTech, 3:8, "tycoonFeedback", "tech") })
  observeEvent(input$advertise, { # ... (advertise observer remains the same) ...
    if (tycoonState$money >= 50) {
      tycoonState$money <- tycoonState$money - 50; reputationGain <- sample(5:10, 1)
      tycoonState$reputation <- min(tycoonState$reputation + reputationGain, 100)
      customerGain <- length(tycoonState$storeItems) * sample(1:3, 1); tycoonState$customers <- tycoonState$customers + customerGain
      html("tycoonFeedback", paste0("<div class='alert alert-info'>Advertisement! Rep +", reputationGain, ", Customers +", customerGain, "</div>")); delay(4000, html("tycoonFeedback", ""))
    } else { html("tycoonFeedback", "<div class='alert alert-danger'>Not enough money!</div>"); delay(3000, html("tycoonFeedback", "")) } })
  observeEvent(input$nextDay, { # ... (nextDay observer remains the same) ...
    itemCount <- length(tycoonState$storeItems)
    if (itemCount == 0) { html("tycoonFeedback", "<div class='alert alert-warning'>Store empty!</div>"); delay(3000, html("tycoonFeedback", "")); return() }
    avgSpending <- 8; reputationMod <- 1 + (tycoonState$reputation / 150); itemVarietyMod <- 1 + (min(itemCount, 10) / 20)
    revenue <- round(tycoonState$customers * avgSpending * reputationMod * itemVarietyMod)
    tycoonState$money <- tycoonState$money + revenue; tycoonState$day <- tycoonState$day + 1
    tycoonState$customers <- max(0, round(tycoonState$customers * runif(1, 0.75, 0.95)))
    tycoonState$reputation <- max(0, round(tycoonState$reputation * runif(1, 0.90, 0.98)))
    html("tycoonFeedback", paste0("<div class='alert alert-info'><strong>Day ", tycoonState$day, "!</strong> Revenue: $", revenue, ". Cust: ", tycoonState$customers, ". Rep: ", tycoonState$reputation, "</div>"))
    gameScores$tycoon <- gameScores$tycoon + round(revenue / 50) })
  updateStoreDisplay <- function() { # ... (updateStoreDisplay function remains the same) ...
    items <- tycoonState$storeItems
    if (length(items) == 0) { html("storeDisplay", "<p class='text-center text-muted'>(Store empty)</p>")
    } else { itemsHTML <- sapply(items, function(item) { paste0("<span class='store-item'>", item, "</span>") }); html("storeDisplay", paste(itemsHTML, collapse = " ")) } }
  
  
  # ---- STARTUP INITIALIZATION ----
  # This observer runs ONCE when the session starts and client data is available
  observeEvent(session$clientData$url_hostname, {
    # Ensure this block runs only once using the flag
    if (!hasInitialized()) {
      # 1. Initialize Store Display
      updateStoreDisplay()
      
      # 2. Animate the initial tab
      req(input$tabs) # Make sure the initial tab value is registered
      initial_tab_content_id <- paste0("content_", input$tabs)
      triggerAnimation(initial_tab_content_id)
      
      # 3. Load initial trivia question if starting on trivia tab
      if(input$tabs == "trivia"){
        loadTriviaQuestion()
      }
      
      # Set the flag to TRUE so this doesn't run again
      hasInitialized(TRUE)
    }
  }, once = TRUE) # observeEvent's once=TRUE is generally reliable here
  
  
  # ---- TAB SWITCHING & ANIMATIONS ----
  observeEvent(input$tabs, {
    req(input$tabs) # Ensure tab value is available
    # Don't run animation logic if this is the very first tab load (handled by startup observer)
    if(hasInitialized()){
      current_tab <- input$tabs
      content_id <- paste0("content_", current_tab)
      
      # Trigger animation for the selected tab's content
      triggerAnimation(content_id)
      
      # Stop click race timer if switching away
      if (current_tab != "clickrace" && timer_running()) {
        timer_running(FALSE) # Stop the timer loop
        raceState$active <- FALSE
        enable("startRace")
        html("raceFeedback", "<div class='alert alert-warning'>Race paused.</div>")
      }
      
      # Reset certain UI states or clear feedback on tab switch if desired
      if (current_tab == "puzzle") html("puzzleFeedback", "")
      if (current_tab == "match") html("matchFeedback", "")
      if (current_tab == "tycoon") html("tycoonFeedback", "") # Keep tycoon feedback maybe?
      
      # Load initial trivia question if switching to trivia and not loaded yet
      if(current_tab == "trivia" && is.null(currentQuestionData())){
        loadTriviaQuestion()
      }
    }
  }, ignoreInit = TRUE) # Important: Prevents running before startup observer
  
} # End server function

# --- Run the Application ---
shinyApp(ui = ui, server = server)
