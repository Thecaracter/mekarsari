checkLogin <- function(){
  session$sendCustomMessage("checkLogin", list())
}

observeEvent(input$stored_user, {
  if (!is.null(input$stored_user)) {
    shinyjs::show("openFormProfilDesa")
    shinyjs::show("openFormProfilDesa2")
    shinyjs::show("openFormProfilDesa3")
    shinyjs::show("openFormProfilDesa4")
    shinyjs::show("openFormProfilDesa5")
    shinyjs::show("openFormProfilDesa6")
  } else {
    shinyjs::hide("openFormProfilDesa")
    shinyjs::hide("openFormProfilDesa2")
    shinyjs::hide("openFormProfilDesa3")
    shinyjs::hide("openFormProfilDesa4")
    shinyjs::hide("openFormProfilDesa5")
    shinyjs::hide("openFormProfilDesa6")
  }
})

loginText <- function(){
  output$loginTextOutput <- renderUI({
    if (!is.null(input$stored_user)) {
      tags$span("Logout", style = "display: inline;")
    } else {
      tags$span("Login", style = "display: inline;")
    }
    
  })
}

loginText()
checkLogin()

observeEvent(input$loginBtn, {
  showModal(modalDialog(
    title = "Loading...",
    "Proses verifikasi data",
    easyClose = FALSE,
    footer = NULL
  ))
  data <- read.csv('data/users.csv')
  
  if(input$username == data[2] && input$password == data[3]){
    session$sendCustomMessage("storeLogin", data)
    loginText()
    checkLogin()
    removeModal()
    showModal(modalDialog(
      title = "Success",
      "Login berhasil",
      easyClose = TRUE,
      footer = NULL
    ))
    session$reload()
  } else {
    loginTextOutput()
    showModal(modalDialog(
      title = "Success",
      "Login gagal",
      easyClose = TRUE,
      footer = NULL
    ))
  }
})


observeEvent(input$logoutBtn, {
  showModal(modalDialog(
    title = "Loading...",
    "Proses logout",
    easyClose = FALSE,
    footer = NULL
  ))
  session$sendCustomMessage("clearLogin", list())
  showModal(modalDialog(
    title = "Success",
    "Logout berhasil",
    easyClose = TRUE,
    footer = NULL
  ))
  session$reload()
})