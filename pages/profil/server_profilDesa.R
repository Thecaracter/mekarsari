render_server_profil_desa <- function(params) {
  
  safeWriteCSV <- function(data, path) {
    tryCatch({
      write.csv(data, path, row.names = FALSE)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }
  
  pathTambahProfilDesa <- "data/profil_desa.csv"
  data_profil_desa <- read_csv("data/profil_desa.csv")
  
  pathTambahProfilDesaBantarKulon <- "data/profil_desa_bantarkulon.csv"
  data_profil_desa_bantarkulon <- read_csv("data/profil_desa_bantarkulon.csv")
  
  
  pathTambahProfilDesaCibarengkok <- "data/profil_desa_cibarengkok.csv"
  data_profil_desa_cibarengkok <- read_csv("data/profil_desa_cibarengkok.csv")
  
  pathTambahProfilDesaCibitung <- "data/profil_desa_cibitung.csv"
  data_profil_desa_cibitung <- read_csv("data/profil_desa_cibitung.csv")
  
  
  pathTambahProfilDesaCibunian <- "data/profil_desa_cibunian.csv"
  data_profil_desa_cibunian <- read_csv("data/profil_desa_cibunian.csv")
  
  pathTambahProfilDesaGarutMukti <- "data/profil_desa_garut_garumukti.csv"
  data_profil_desa_garutmukti <- read_csv("data/profil_desa_garut_garumukti.csv")
  
  
  identifikasi <- function(id){
    if(id == '1'){
      pathTambahProfilDesa
    }
    else if(id == '2'){
      pathTambahProfilDesaBantarKulon
    }
    else if(id == '3'){
      pathTambahProfilDesaCibarengkok
    }
    else if(id == '4'){
      pathTambahProfilDesaCibitung
    }
    else if(id == '5'){
      pathTambahProfilDesaCibunian
    }
    else if(id == '6'){
      pathTambahProfilDesaGarutMukti
    }
  }
  
  print(data_profil_desa)
  
  resetInputs <- function() {
    updateTextInput(session,"nama", value="")	
    updateTextInput(session,"alamat", value="")	
    updateTextInput(session,"timur", value="")	
    updateTextInput(session,"selatan", value="")	
    updateTextInput(session,"barat", value="")	
    updateTextInput(session,"utara", value="")	
    updateNumericInput(session,"laki.laki", value=0)	
    updateNumericInput(session,"perempuan", value=0)	
    updateNumericInput(session,"kepala.keluarga", value=0)	
    updateTextInput(session,"sejarah", value="")	
    updateTextInput(session,"kepala.desa", value="")
  }
  
  loadDataProfilDesa <- function(alamat) {
    if (file.exists(alamat)) {
      read.csv(alamat)
    } else {
      data.frame(
        No = character(),
        nama=character(),	
        alamat=character(),	
        timur=character(),	
        selatan=character(),	
        barat=character(),	
        utara=character(),	
        laki.laki=character(),	
        perempuan=character(),	
        kepala.keluarga=character(),	
        sejarah=character(),	
        kepala.desa=character(),	
        foto.kepala.desa=character()
      )
    }
  }
  
  observeEvent(input$openFormProfilDesa, {
    id <- 1
    data_profil_desa <- loadDataProfilDesa(pathTambahProfilDesa)
    data_profil_desa <- data_profil_desa[data_profil_desa$No == id, ]
    updateTextInput(session,"nama", value=data_profil_desa$nama)	
    updateTextInput(session,"alamat", value=data_profil_desa$alamat)	
    updateTextInput(session,"timur", value=data_profil_desa$timur)	
    updateTextInput(session,"selatan", value=data_profil_desa$selatan)	
    updateTextInput(session,"barat", value=data_profil_desa$barat)	
    updateTextInput(session,"utara", value=data_profil_desa$utara)	
    updateNumericInput(session,"laki.laki", value=data_profil_desa$laki.laki)	
    updateNumericInput(session,"perempuan", value=data_profil_desa$perempuan)	
    updateNumericInput(session,"kepala.keluarga", value=data_profil_desa$kepala.keluarga)	
    updateTextInput(session,"sejarah", value=data_profil_desa$sejarah)	
    updateTextInput(session,"kepala.desa", value=data_profil_desa$kepala.desa)
    
    session$sendCustomMessage("form_profil_desa_update", "1")
    session$sendCustomMessage("identifikasi", "1")
    reset("foto.kepala.desa")
  })
  
  observeEvent(input$openFormProfilDesa2, {
    id <- 1
    data_profil_desa <- loadDataProfilDesa(pathTambahProfilDesaBantarKulon)
    data_profil_desa <- data_profil_desa[data_profil_desa$No == id, ]
    updateTextInput(session,"nama", value=data_profil_desa$nama)	
    updateTextInput(session,"alamat", value=data_profil_desa$alamat)	
    updateTextInput(session,"timur", value=data_profil_desa$timur)	
    updateTextInput(session,"selatan", value=data_profil_desa$selatan)	
    updateTextInput(session,"barat", value=data_profil_desa$barat)	
    updateTextInput(session,"utara", value=data_profil_desa$utara)	
    updateNumericInput(session,"laki.laki", value=data_profil_desa$laki.laki)	
    updateNumericInput(session,"perempuan", value=data_profil_desa$perempuan)	
    updateNumericInput(session,"kepala.keluarga", value=data_profil_desa$kepala.keluarga)	
    updateTextInput(session,"sejarah", value=data_profil_desa$sejarah)	
    updateTextInput(session,"kepala.desa", value=data_profil_desa$kepala.desa)
    
    session$sendCustomMessage("form_profil_desa_update", "1")
    session$sendCustomMessage("identifikasi", "2")
    reset("foto.kepala.desa")
  })
  
  observeEvent(input$openFormProfilDesa3, {
    id <- 1
    data_profil_desa <- loadDataProfilDesa(pathTambahProfilDesaCibarengkok)
    data_profil_desa <- data_profil_desa[data_profil_desa$No == id, ]
    updateTextInput(session,"nama", value=data_profil_desa$nama)	
    updateTextInput(session,"alamat", value=data_profil_desa$alamat)	
    updateTextInput(session,"timur", value=data_profil_desa$timur)	
    updateTextInput(session,"selatan", value=data_profil_desa$selatan)	
    updateTextInput(session,"barat", value=data_profil_desa$barat)	
    updateTextInput(session,"utara", value=data_profil_desa$utara)	
    updateNumericInput(session,"laki.laki", value=data_profil_desa$laki.laki)	
    updateNumericInput(session,"perempuan", value=data_profil_desa$perempuan)	
    updateNumericInput(session,"kepala.keluarga", value=data_profil_desa$kepala.keluarga)	
    updateTextInput(session,"sejarah", value=data_profil_desa$sejarah)	
    updateTextInput(session,"kepala.desa", value=data_profil_desa$kepala.desa)
    
    session$sendCustomMessage("form_profil_desa_update", "1")
    session$sendCustomMessage("identifikasi", "3")
    reset("foto.kepala.desa")
  })
  
  observeEvent(input$openFormProfilDesa4, {
    id <- 1
    data_profil_desa <- loadDataProfilDesa(pathTambahProfilDesaCibitung)
    data_profil_desa <- data_profil_desa[data_profil_desa$No == id, ]
    updateTextInput(session,"nama", value=data_profil_desa$nama)	
    updateTextInput(session,"alamat", value=data_profil_desa$alamat)	
    updateTextInput(session,"timur", value=data_profil_desa$timur)	
    updateTextInput(session,"selatan", value=data_profil_desa$selatan)	
    updateTextInput(session,"barat", value=data_profil_desa$barat)	
    updateTextInput(session,"utara", value=data_profil_desa$utara)	
    updateNumericInput(session,"laki.laki", value=data_profil_desa$laki.laki)	
    updateNumericInput(session,"perempuan", value=data_profil_desa$perempuan)	
    updateNumericInput(session,"kepala.keluarga", value=data_profil_desa$kepala.keluarga)	
    updateTextInput(session,"sejarah", value=data_profil_desa$sejarah)	
    updateTextInput(session,"kepala.desa", value=data_profil_desa$kepala.desa)
    
    session$sendCustomMessage("form_profil_desa_update", "1")
    session$sendCustomMessage("identifikasi", "4")
    reset("foto.kepala.desa")
  })
  
  observeEvent(input$openFormProfilDesa5, {
    id <- 1
    data_profil_desa <- loadDataProfilDesa(pathTambahProfilDesaCibunian)
    data_profil_desa <- data_profil_desa[data_profil_desa$No == id, ]
    updateTextInput(session,"nama", value=data_profil_desa$nama)	
    updateTextInput(session,"alamat", value=data_profil_desa$alamat)	
    updateTextInput(session,"timur", value=data_profil_desa$timur)	
    updateTextInput(session,"selatan", value=data_profil_desa$selatan)	
    updateTextInput(session,"barat", value=data_profil_desa$barat)	
    updateTextInput(session,"utara", value=data_profil_desa$utara)	
    updateNumericInput(session,"laki.laki", value=data_profil_desa$laki.laki)	
    updateNumericInput(session,"perempuan", value=data_profil_desa$perempuan)	
    updateNumericInput(session,"kepala.keluarga", value=data_profil_desa$kepala.keluarga)	
    updateTextInput(session,"sejarah", value=data_profil_desa$sejarah)	
    updateTextInput(session,"kepala.desa", value=data_profil_desa$kepala.desa)
    
    session$sendCustomMessage("form_profil_desa_update", "1")
    session$sendCustomMessage("identifikasi", "5")
    reset("foto.kepala.desa")
  })
  
  observeEvent(input$openFormProfilDesa6, {
    id <- 1
    data_profil_desa <- loadDataProfilDesa(pathTambahProfilDesaGarutMukti)
    data_profil_desa <- data_profil_desa[data_profil_desa$No == id, ]
    updateTextInput(session,"nama", value=data_profil_desa$nama)	
    updateTextInput(session,"alamat", value=data_profil_desa$alamat)	
    updateTextInput(session,"timur", value=data_profil_desa$timur)	
    updateTextInput(session,"selatan", value=data_profil_desa$selatan)	
    updateTextInput(session,"barat", value=data_profil_desa$barat)	
    updateTextInput(session,"utara", value=data_profil_desa$utara)	
    updateNumericInput(session,"laki.laki", value=data_profil_desa$laki.laki)	
    updateNumericInput(session,"perempuan", value=data_profil_desa$perempuan)	
    updateNumericInput(session,"kepala.keluarga", value=data_profil_desa$kepala.keluarga)	
    updateTextInput(session,"sejarah", value=data_profil_desa$sejarah)	
    updateTextInput(session,"kepala.desa", value=data_profil_desa$kepala.desa)
    
    session$sendCustomMessage("form_profil_desa_update", "1")
    session$sendCustomMessage("identifikasi", "6")
    reset("foto.kepala.desa")
  })
  
  observeEvent(input$cancelProfilDesa, {
    session$sendCustomMessage("form_profil_desa_update", "0")
    # shinyjs::show("openFormProfilDesa")
    # shinyjs::show("openFormProfilDesa2")
    # shinyjs::show("openFormProfilDesa3")
    # shinyjs::show("openFormProfilDesa4")
    # shinyjs::show("openFormProfilDesa5")
    # shinyjs::show("openFormProfilDesa6")
  })
  
  observeEvent(input$updateProfilDesa, {
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data_profil_desa <- loadDataProfilDesa(identifikasi(input$identifikasi))
      
      file_info <- input$foto.kepala.desa
      file_name <- data_profil_desa$foto.kepala.desa
      if (!is.null(file_info)){
        file_path <- file_info$datapath
        file_name <- file_info$name
        
        save_path <- file.path("www", file_name)
        
        file.copy(file_path, save_path)
      }
      
      data_profil_desa[data_profil_desa$No == "1", ] <- data.frame(
        No = "1",
        nama=input$nama,	
        alamat=input$alamat,	
        timur=input$timur,	
        selatan=input$selatan,	
        barat=input$barat,	
        utara=input$utara,	
        laki.laki=input$laki.laki,	
        perempuan=input$perempuan,	
        kepala.keluarga=input$kepala.keluarga,	
        sejarah=input$sejarah,	
        kepala.desa=input$kepala.desa,	
        foto.kepala.desa=file_name,
        stringsAsFactors = FALSE
      )
      
      successProfilDesa <- safeWriteCSV(data_profil_desa, paste0(identifikasi(input$identifikasi), ".tmp"))
      
      if (successProfilDesa){
        file.rename(paste0(identifikasi(input$identifikasi), ".tmp"), identifikasi(input$identifikasi))
        resetInputs()
        
        session$sendCustomMessage("form_profil_desa_update", "0")
        shinyjs::show("openFormProfilDesa")
        
        reset("foto.kepala.desa")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
        
        session$reload()
        
        render_server_profil_desa(FALSE)
      }else {
        
        unlink(paste0(identifikasi(input$identifikasi), ".tmp"))
        
        removeModal()
        
        showModal(modalDialog(
          title = "Error",
          "Gagal menambahkan data.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    params = TRUE
  })
  
  output$selamatDatang <- renderText({
    selamatDatang <- paste("Di ", data_profil_desa$nama, ", ", data_profil_desa$alamat)
    selamatDatang
  })
  
  output$desa <- renderText({
    selamatDatang <- paste(data_profil_desa$nama)
    selamatDatang
  })
  
  output$sejarahDesa <- renderText({
    sejarahDesa <- paste(data_profil_desa$sejarah)
    sejarahDesa
  })
  
  output$batasTimur <- renderText({
    batasTimur <- paste( data_profil_desa$timur)
    batasTimur
  })
  
  output$batasSelatan <- renderText({
    batasSelatan <- paste( data_profil_desa$selatan)
    batasSelatan
  })
  
  output$batasUtara <- renderText({
    batasUtara <- paste( data_profil_desa$utara)
    batasUtara
  })
  
  output$batasBarat <- renderText({
    batasBarat <- paste(data_profil_desa$barat)
    batasBarat
  })
  
  output$lakiPerempuan <- renderPlot({
    df <- data.frame(
      Gender = c("Laki-laki", "Perempuan"),
      Count = c(data_profil_desa$laki.laki, data_profil_desa$perempuan)
    )
    
    ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "red")) +
      theme_minimal() +
      labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
  })
  
  output$kepalaKeluarga <- renderText({
    kepalaKeluarga <- paste(data_profil_desa$kepala.keluarga, "Kepala Keluarga")
    kepalaKeluarga
  })
  
  output$kepalaDesa <- renderText({
    kepalaDesa <- paste(data_profil_desa$kepala.desa)
    kepalaDesa
  })
  
  output$fotoKepalaDesa <- renderUI({
    fotoKepalaDesa <- data_profil_desa$foto.kepala.desa
    img(src = fotoKepalaDesa, width = "100%")
  })
  
  
  #Desa Bantar Kulon
  print(data_profil_desa_bantarkulon)
  
  output$desa2 <- renderText({
    selamatDatang <- paste(data_profil_desa_bantarkulon$nama)
    selamatDatang
  })
  
  output$selamatDatang2 <- renderText({
    selamatDatang <- paste("Di ", data_profil_desa_bantarkulon$nama, ", ", data_profil_desa_bantarkulon$alamat)
    selamatDatang
  })
  
  output$sejarahDesa2 <- renderText({
    sejarahDesa <- paste(data_profil_desa_bantarkulon$sejarah)
    sejarahDesa
  })
  
  output$batasTimur2 <- renderText({
    batasTimur <- paste( data_profil_desa_bantarkulon$timur)
    batasTimur
  })
  
  output$batasSelatan2 <- renderText({
    batasSelatan <- paste( data_profil_desa_bantarkulon$selatan)
    batasSelatan
  })
  
  output$batasUtara2 <- renderText({
    batasUtara <- paste( data_profil_desa_bantarkulon$utara)
    batasUtara
  })
  
  output$batasBarat2 <- renderText({
    batasBarat <- paste(data_profil_desa_bantarkulon$barat)
    batasBarat
  })
  
  output$lakiPerempuan2 <- renderPlot({
    df <- data.frame(
      Gender = c("Laki-laki", "Perempuan"),
      Count = c(data_profil_desa_bantarkulon$laki.laki, data_profil_desa_bantarkulon$perempuan)
    )
    
    ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "red")) +
      theme_minimal() +
      labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
  })
  
  output$kepalaKeluarga2 <- renderText({
    kepalaKeluarga <- paste(data_profil_desa_bantarkulon$kepala.keluarga, "Kepala Keluarga")
    kepalaKeluarga
  })
  
  output$kepalaDesa2 <- renderText({
    kepalaDesa <- paste(data_profil_desa_bantarkulon$kepala.desa)
    kepalaDesa
  })
  
  output$fotoKepalaDesa2 <- renderUI({
    fotoKepalaDesa <- data_profil_desa_bantarkulon$foto.kepala.desa
    img(src = fotoKepalaDesa, width = "100%")
  })
  
  #Desa Cibarengkok
  print(data_profil_desa_cibarengkok)
  
  output$desa3 <- renderText({
    selamatDatang <- paste(data_profil_desa_cibarengkok$nama)
    selamatDatang
  })
  
  output$selamatDatang3 <- renderText({
    selamatDatang <- paste("Di ", data_profil_desa_cibarengkok$nama, ", ", data_profil_desa_cibarengkok$alamat)
    selamatDatang
  })
  
  output$sejarahDesa3 <- renderText({
    sejarahDesa <- paste(data_profil_desa_cibarengkok$sejarah)
    sejarahDesa
  })
  
  output$batasTimur3 <- renderText({
    batasTimur <- paste( data_profil_desa_cibarengkok$timur)
    batasTimur
  })
  
  output$batasSelatan3 <- renderText({
    batasSelatan <- paste( data_profil_desa_cibarengkok$selatan)
    batasSelatan
  })
  
  output$batasUtara3 <- renderText({
    batasUtara <- paste( data_profil_desa_cibarengkok$utara)
    batasUtara
  })
  
  output$batasBarat3 <- renderText({
    batasBarat <- paste(data_profil_desa_cibarengkok$barat)
    batasBarat
  })
  
  output$lakiPerempuan3 <- renderPlot({
    df <- data.frame(
      Gender = c("Laki-laki", "Perempuan"),
      Count = c(data_profil_desa_cibarengkok$laki.laki, data_profil_desa_cibarengkok$perempuan)
    )
    
    ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "red")) +
      theme_minimal() +
      labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
  })
  
  output$kepalaKeluarga3 <- renderText({
    kepalaKeluarga <- paste(data_profil_desa_cibarengkok$kepala.keluarga, "Kepala Keluarga")
    kepalaKeluarga
  })
  
  output$kepalaDesa3 <- renderText({
    kepalaDesa <- paste(data_profil_desa_cibarengkok$kepala.desa)
    kepalaDesa
  })
  
  output$fotoKepalaDesa3 <- renderUI({
    fotoKepalaDesa <- data_profil_desa_cibarengkok$foto.kepala.desa
    img(src = fotoKepalaDesa, width = "100%")
  })
  
  
  #Desa Cibitung
  print(data_profil_desa_cibitung)
  
  output$desa4 <- renderText({
    selamatDatang <- paste(data_profil_desa_cibitung$nama)
    selamatDatang
  })
  
  output$selamatDatang4 <- renderText({
    selamatDatang <- paste("Di ", data_profil_desa_cibitung$nama, ", ", data_profil_desa_cibitung$alamat)
    selamatDatang
  })
  
  output$sejarahDesa4 <- renderText({
    sejarahDesa <- paste(data_profil_desa_cibitung$sejarah)
    sejarahDesa
  })
  
  output$batasTimur4 <- renderText({
    batasTimur <- paste( data_profil_desa_cibitung$timur)
    batasTimur
  })
  
  output$batasSelatan4 <- renderText({
    batasSelatan <- paste( data_profil_desa_cibitung$selatan)
    batasSelatan
  })
  
  output$batasUtara4 <- renderText({
    batasUtara <- paste( data_profil_desa_cibitung$utara)
    batasUtara
  })
  
  output$batasBarat4 <- renderText({
    batasBarat <- paste(data_profil_desa_cibitung$barat)
    batasBarat
  })
  
  output$lakiPerempuan4 <- renderPlot({
    df <- data.frame(
      Gender = c("Laki-laki", "Perempuan"),
      Count = c(data_profil_desa_cibitung$laki.laki, data_profil_desa_cibitung$perempuan)
    )
    
    ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "red")) +
      theme_minimal() +
      labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
  })
  
  output$kepalaKeluarga4 <- renderText({
    kepalaKeluarga <- paste(data_profil_desa_cibitung$kepala.keluarga, "Kepala Keluarga")
    kepalaKeluarga
  })
  
  output$kepalaDesa4 <- renderText({
    kepalaDesa <- paste(data_profil_desa_cibitung$kepala.desa)
    kepalaDesa
  })
  
  output$fotoKepalaDesa4 <- renderUI({
    fotoKepalaDesa <- data_profil_desa_cibitung$foto.kepala.desa
    img(src = fotoKepalaDesa, width = "100%")
  })
  
  #Desa Cibunian
  print(data_profil_desa_cibunian)
  
  output$desa5 <- renderText({
    selamatDatang <- paste(data_profil_desa_cibunian$nama)
    selamatDatang
  })
  
  output$selamatDatang5 <- renderText({
    selamatDatang <- paste("Di ", data_profil_desa_cibunian$nama, ", ", data_profil_desa_cibunian$alamat)
    selamatDatang
  })
  
  output$sejarahDesa5 <- renderText({
    sejarahDesa <- paste(data_profil_desa_cibunian$sejarah)
    sejarahDesa
  })
  
  output$batasTimur5 <- renderText({
    batasTimur <- paste( data_profil_desa_cibunian$timur)
    batasTimur
  })
  
  output$batasSelatan5 <- renderText({
    batasSelatan <- paste( data_profil_desa_cibunian$selatan)
    batasSelatan
  })
  
  output$batasUtara5 <- renderText({
    batasUtara <- paste( data_profil_desa_cibunian$utara)
    batasUtara
  })
  
  output$batasBarat5 <- renderText({
    batasBarat <- paste(data_profil_desa_cibunian$barat)
    batasBarat
  })
  
  output$lakiPerempuan5 <- renderPlot({
    df <- data.frame(
      Gender = c("Laki-laki", "Perempuan"),
      Count = c(data_profil_desa_cibunian$laki.laki, data_profil_desa_cibunian$perempuan)
    )
    
    ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "red")) +
      theme_minimal() +
      labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
  })
  
  output$kepalaKeluarga5 <- renderText({
    kepalaKeluarga <- paste(data_profil_desa_cibunian$kepala.keluarga, "Kepala Keluarga")
    kepalaKeluarga
  })
  
  output$kepalaDesa5 <- renderText({
    kepalaDesa <- paste(data_profil_desa_cibunian$kepala.desa)
    kepalaDesa
  })
  
  output$fotoKepalaDesa5 <- renderUI({
    fotoKepalaDesa <- data_profil_desa_cibunian$foto.kepala.desa
    img(src = fotoKepalaDesa, width = "100%")
  })
  
  #Desa Garut Mukti
  print(data_profil_desa_garutmukti)
  
  output$desa6 <- renderText({
    selamatDatang <- paste(data_profil_desa_garutmukti$nama)
    selamatDatang
  })
  
  output$selamatDatang6 <- renderText({
    selamatDatang <- paste("Di ", data_profil_desa_garutmukti$nama, ", ", data_profil_desa_garutmukti$alamat)
    selamatDatang
  })
  
  output$sejarahDesa6 <- renderText({
    sejarahDesa <- paste(data_profil_desa_garutmukti$sejarah)
    sejarahDesa
  })
  
  output$batasTimur6 <- renderText({
    batasTimur <- paste( data_profil_desa_garutmukti$timur)
    batasTimur
  })
  
  output$batasSelatan6 <- renderText({
    batasSelatan <- paste( data_profil_desa_garutmukti$selatan)
    batasSelatan
  })
  
  output$batasUtara6 <- renderText({
    batasUtara <- paste( data_profil_desa_garutmukti$utara)
    batasUtara
  })
  
  output$batasBarat6 <- renderText({
    batasBarat <- paste(data_profil_desa_garutmukti$barat)
    batasBarat
  })
  
  output$lakiPerempuan6 <- renderPlot({
    df <- data.frame(
      Gender = c("Laki-laki", "Perempuan"),
      Count = c(data_profil_desa_garutmukti$laki.laki, data_profil_desa_garutmukti$perempuan)
    )
    
    ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "red")) +
      theme_minimal() +
      labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
  })
  
  output$kepalaKeluarga6 <- renderText({
    kepalaKeluarga <- paste(data_profil_desa_garutmukti$kepala.keluarga, "Kepala Keluarga")
    kepalaKeluarga
  })
  
  output$kepalaDesa6 <- renderText({
    kepalaDesa <- paste(data_profil_desa_garutmukti$kepala.desa)
    kepalaDesa
  })
  
  output$fotoKepalaDesa6 <- renderUI({
    fotoKepalaDesa <- data_profil_desa_garutmukti$foto.kepala.desa
    img(src = fotoKepalaDesa, width = "100%")
  })
}

render_server_profil_desa(TRUE)