library(shiny)
library(firebase)
library(DT)
library(nycflights13)
library(data.table)

# Load the flights dataset
data("flights")

# Convert flights data.frame to data.table
setDT(flights)

library(RMySQL)

# Database configuration
db_config <- list(
  host = "localhost",
  user = "root",
  password = "1234",
  port = 3306,
  dbname = "security"
)

# Connect to the database with local_infile parameter enabled
conn <- dbConnect(MySQL(), host = db_config$host, user = db_config$user, 
                  password = db_config$password, dbname = db_config$dbname,
                  local_infile = TRUE)

# Execute a query to fetch data
query <- "SELECT * FROM user_data"
user_data <- dbGetQuery(conn, query)

# Login modal
login_modal <- modalDialog(
  title = "Sign in",
  textInput("email_signin", "Your email"),
  passwordInput("password_signin", "Your password"),
  footer = actionButton("signin", "Sign in")
)

# Registration modal
register_modal <- modalDialog(
  title = "Register",
  useFirebase(),
  textInput("email", "Your email"),
  passwordInput("password", "Choose your password"),
  actionButton("submit", "Register")
)

# Modal for disabled account message
disabled_modal <- modalDialog(
  title = "Account Disabled",
  "Hello! Please wait for admin approval to access the application."
)

# Access restricted modal
access_restricted_modal <- modalDialog(
  title = "Access Restricted",
  "You do not have permission to access User Management.Browse the plots "
)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        /* Apply pastel blue color to the entire UI page */
        body {
          background-color: #add8e6; /* pastel blue */
        }
        
        /* Apply grey color to the navbar */
        .navbar-default {
          background-color:  #f0f8ff;; /* grey */
        }
        .button-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .button-row .button-container {
        flex: 1; /* Make both buttons occupy equal space */
        text-align: center; /* Center the buttons */
      }
      .button-row #delete_button_ui button {
        background-color: #dc3545; /* Red background color */
      }
      .button-row #enable_button_ui button {
        background-color: #28a745; /* Green background color */
      }
      ")
    )
  ),
  useFirebase(),
  navbarPage(
    title = tags$span("ActServ", class = "icon", icon("info-circle")), 
    tabPanel(title = tags$span(icon("user"), "Login"), 
             uiOutput("auth_buttons"), 
             uiOutput("sidebar_ui"),
             mainPanel(
               dataTableOutput("filtered_table"),
               verbatimTextOutput("summary")
             )
    ),
    tabPanel(title = tags$span(icon("cog"), "Admin"),
             fluidRow(
               dataTableOutput("user_table")
               ),
             fluidRow(
               div(
                 class = "button-row",
                 div(class = "button-container", uiOutput("enable_button_ui")),
                 div(class = "button-container", uiOutput("delete_button_ui"))
               )
             )
    )
  )
)

server <- function(input, output, session) {
  f <- FirebaseEmailPassword$new()
  
  # Reactive variable to track login status
  logged_in <- reactiveVal(FALSE)
  user_role <- reactiveVal("none")
  
  # Reactive expression to filter flights based on user inputs
  filtered_flights <- reactive({
    filtered <- flights
    if (!is.null(input$origin)) {
      filtered <- filtered[origin == input$origin]
    }
    if (!is.null(input$dest)) {
      filtered <- filtered[dest == input$dest]
    }
    if (!is.null(input$carrier)) {
      filtered <- filtered[carrier == input$carrier]
    }
    filtered
  })
  
  # Open login modal
  observeEvent(input$login_modal, {
    showModal(login_modal)
  })
  
  # Open registration modal
  observeEvent(input$register_modal, {
    showModal(register_modal)
  })
  
  # Login user
  observeEvent(input$signin, {
    email <- input$email_signin
    password <- input$password_signin
    
    if (email == "" || password == "") {
      return() # Handle empty email or password case
    }
    
    # Sign in user with provided credentials
    f$sign_in(email, password)
    
    observeEvent(f$get_signed_in(), {
      signed_in <- f$get_signed_in()
      if (signed_in$success) {
        user_index <- which(user_data$user == email)
        if (length(user_index) > 0) {
          user_status <- user_data$Status[user_index]
          if (user_status == "Active") {
            showNotification("Logged in successfully!", type = "message")
            removeModal() # Close login modal after successful login
            logged_in(TRUE) # Update login status
            
            # Apply custom styles to the table header
            output$filtered_table <- renderDT({
              excluded_columns <- c("year", "tailnum","dep_time","sched_dep_time","arr_time","sched_arr_time","carrier","origin","dest")  
              columns <- setdiff(names(flights), excluded_columns)  # Exclude specified columns
              
              # Modify the month column to display month names
              filtered_data <- filtered_flights()[, ..columns]
              filtered_data$month <- month.name[filtered_data$month]
              
              datatable(
                filtered_data,  # Subset the data to selected columns
                options = list(pageLength = 5),
                class = 'cell-border stripe hover',
                style = 'bootstrap',
                filter = 'top',
                extensions = c('Buttons', 'ColReorder', 'FixedHeader'),
                rownames = FALSE
              ) %>%
                formatStyle(columns, fontWeight = 'bold', backgroundColor = 'lightblue')  # Styling the header
            })
            
            # Render summary statistics
            output$summary <- renderText({
              filtered_data <- filtered_flights()  # Get the filtered data
              
              summary_data <- filtered_data[, .(
                Total_flights = .N,
                Average_delay = mean(dep_delay, na.rm = TRUE),
                Max_delay = max(dep_delay, na.rm = TRUE),
                Min_delay = min(dep_delay, na.rm = TRUE)
              )]
              
              # Extract the filtered airline (if any)
              filtered_airline <- unique(filtered_data$carrier)
              
              # Construct the summary header including the filtered airline
              if (!is.null(filtered_airline)) {
                summary_text <- paste("Summary of", filtered_airline, "flights\n")
              } else {
                summary_text <- "Summary:\n"
              }
              
              # Construct the summary text including the summary statistics
              summary_text <- paste(summary_text, paste(names(summary_data), summary_data, sep = ": ", collapse = "\n"), sep = "\n")
              
              # Return the summary text
              summary_text
            })
            
            # Check if the user is admin
            if (user_data$Role[user_index] == "admin") {
              # Render user table if the user is admin
              output$user_table <- renderDataTable({
                user_data
              })
              
              # Enable disabled users button UI for admin
              output$enable_button_ui <- renderUI({
                actionButton("enable_button", "Enable Disabled Users")
              })
              
              # Enable disabled users
              observeEvent(input$enable_button, {
                user_data$Status[user_data$Status == "Disabled"] <- "Active"
                output$user_table <- renderDataTable({
                  user_data
                })
                
                # Save updated user data
                # Update the MySQL database with the modified data
                dbWriteTable(conn, "user_data", user_data, overwrite = TRUE)
              })
              
              # Delete users button UI for admin
              output$delete_button_ui <- renderUI({
                actionButton("delete_button", "Delete Users")
              })
              
              # Delete users
              observeEvent(input$delete_button, {
                # Remove selected users
                user_data <- user_data[-input$user_table_rows_selected, ]
                # Re-render user table
                output$user_table <- renderDataTable({
                  user_data
                })
                
                # Delete selected users from the MySQL database
                selected_users <- user_data[input$user_table_rows_selected, ]
                selected_user_emails <- selected_users$user
                
                # Construct DELETE query to remove selected users from the database
                delete_query <- sprintf("DELETE FROM user_data WHERE user IN ('%s')", paste(selected_user_emails, collapse = "','"))
                
                # Execute the DELETE query
                dbSendQuery(conn, delete_query)
              })
              
              
            } else {
              # For non-admin users, do not render the enable button UI
              output$enable_button_ui <- renderUI({
                NULL
              })
              # Show access restricted modal if the user is not an admin
              showModal(access_restricted_modal)
            }
          } else if (user_status == "Disabled") {
            showModal(disabled_modal) # Show the modal for disabled accounts
          }
        } else {
          # If the user is authenticated but not found in user_data, show the disabled modal
          showModal(disabled_modal)
        }
      } else {
        showNotification("Error signing in!", type = "error")
        print(signed_in) # Check for detailed error information
      }
    })
  })
  
  # Register user
  observeEvent(input$submit, {
    email <- input$email
    password <- input$password
    
    if (email == "" || password == "") {
      return()
    }
    
    # Create user with user-provided password
    f$create(email, password)
    
    observeEvent(f$get_created(), {
      created <- f$get_created()
      if (created$success) {
        # Send verification email
        f$send_verification_email()
        showNotification("Verification email sent to your email!", type = "message")
        removeModal()
      } else {
        showNotification("Error creating user!", type = "error")
        print(created)
      }
    })
  })
  
  # Logout user
  observeEvent(input$logout_button, {
    f$sign_out() # Sign out user
    logged_in(FALSE) # Update login status
    
    # Reset table outputs
    output$filtered_table <- renderDataTable(NULL)
    output$user_table <- renderDataTable(NULL)
    output$summary <- renderText(NULL)
    output$enable_button_ui <- renderUI(NULL)
  })
  
  output$auth_buttons <- renderUI({
    if (!logged_in()) {
      tagList(
        div(
          style = "text-align: center; margin-top: 30px;",
          actionButton("login_modal", 
                       "Sign in", 
                       style = "color: #fff; background-color: #007bff; border-color: #007bff;"),
          actionButton("register_modal", "Register", 
                       style = "color: #fff; background-color: #007bff; border-color: #007bff;")
        )
      )
    } else {
      actionButton("logout_button", label = icon("sign-out"), style = "color: #fff; background-color: #007bff; border-color: #007bff; float: right;margin-bottom: 20px;")
    }
  })
  
  output$enable_button_ui <- renderUI({
    if (logged_in()) {
      actionButton("enable_button", "Enable Disabled Users", 
                   style = "color: #fff; background-color: #007bff; border-color: #007bff;")
      
    } else {
      NULL
    }
  })
  
  output$sidebar_ui <- renderUI({
    sidebarPanel(
      selectInput("origin", "Origin Airport:", choices = unique(flights$origin)),
      selectInput("dest", "Destination Airport:", choices = unique(flights$dest)),
      selectInput("carrier", "Airline:", choices = unique(flights$carrier)),
      div(style="display: flex; flex-direction: row; align-items: center;",
          actionButton("filter", "Apply Filters"),
          actionButton("reset", "Reset Filters")
      ),
      style = if (logged_in()) "display: block; background-color: #B3DCD6;" else "display: none;" # Hide/show sidebar based on login status
    )
  })
  
  # Reset filters for sidebar buttons
  observeEvent(input$reset, {
    updateSelectInput(session, "origin", choices = unique(flights$origin), selected = NULL)
    updateSelectInput(session, "dest", choices = unique(flights$dest), selected = NULL)
    updateSelectInput(session, "carrier", choices = unique(flights$carrier), selected = NULL)
  })
  
  
}

shinyApp(ui, server)
