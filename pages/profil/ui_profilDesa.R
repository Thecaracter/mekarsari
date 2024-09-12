tabItemProfilDesa <- tabItem(tabName = "dashboard",
                             tags$script(
                               HTML(
                                 "
      Shiny.addCustomMessageHandler('form_profil_desa_update', function(message) {
        Shiny.setInputValue('form_profil_desa_update', message);
      });
      
      Shiny.addCustomMessageHandler('identifikasi', function(message) {
        Shiny.setInputValue('identifikasi', message);
      });
      
      Shiny.addCustomMessageHandler('resetFileInputHandler', function(message) {
        document.getElementById('foto.kepala.desa').value = null;
      });
    "
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.form_profil_desa_update == '1'", 
                               fluidRow(
                                 box(
                                   title = h3(tags$b("Update Profil Desa")),
                                   status = "primary",
                                   width = 12,
                                   solidHeader = TRUE,
                                   box(
                                     status = "success",
                                     width = 12,
                                     h4(tags$b("Potensi Desa")),
                                     textInput("nama", "Nama"),	
                                     textInput("kepala.desa", "Kepala Desa"),	
                                     fileInput("foto.kepala.desa", "Foto Kepala Desa", 
                                               accept = c("image/png", "image/jpeg", "image/jpg")),
                                     textInput("alamat", "Alamat"),	
                                     textInput("timur", "Timur"),	
                                     textInput("selatan", "Selatan"),	
                                     textInput("barat", "Barat"),	
                                     textInput("utara", "Utara"),	
                                     numericInput("laki.laki", "Jumlah Laki-Laki", value = 0),	
                                     numericInput("perempuan", "Jumlah Perempuan", value = 0),	
                                     numericInput("kepala.keluarga", "Jumlah Kepala Keluarga", value = 0),	
                                     textInput("sejarah", "Sejarah")
                                     
                                   ),
                                   actionButton("updateProfilDesa", "Update"),
                                   actionButton("cancelProfilDesa", "Tutup")
                                 )
                               )
                             ),
                             fluidRow(box(
                               solidHeader = TRUE,
                               status = "primary",
                               width = 12,
                               h2(tags$b("Selamat Datang")),               
                               tabsetPanel(
                                 # Tab Pertama
                                 tabPanel(textOutput("desa"), 
                                          actionButton("openFormProfilDesa", "Perbarui Profil Desa", style = "margin-bottom: 10px;display:none;"),
                                          h2(tags$b(textOutput("selamatDatang"))),
                                          fluidRow(
                                            box(
                                              solidHeader = TRUE,
                                              status = "warning",
                                              h3(tags$b("Sejarah Desa")),
                                              h4(tags$p(textOutput("sejarahDesa")))
                                            ),
                                            box(
                                              solidHeader = TRUE,
                                              status = "danger",
                                              h3(tags$b("Batas Wilayah Desa")),
                                              h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur"))),
                                              h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat"))),
                                              h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan"))),
                                              h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara"))),
                                            )
                                          ),
                                          fluidRow(box(
                                            title = h3(tags$b("Persebaran Laki-laki dan Perempuan")),
                                            solidHeader = TRUE,
                                            width = 12,
                                            box(
                                              solidHeader = TRUE,
                                              width = 4,
                                              h4(
                                                tags$b("Total Kepala Keluarga : ") ,
                                                textOutput("kepalaKeluarga")
                                              )
                                            ),
                                            fluidRow(
                                              box(
                                                solidHeader = TRUE,
                                                width = 8,
                                                plotOutput("lakiPerempuan")
                                              ),
                                              box(
                                                solidHeader = TRUE,
                                                width = 4,
                                                h4(tags$b("Kepala Desa Saat Ini : ")),
                                                uiOutput("fotoKepalaDesa"),
                                                div(h4(tags$b(textOutput("kepalaDesa"))), style = "text-align: center;"),
                                                
                                              )
                                            )
                                          ))
                                 ),
                                 # Tab Kedua
                                 tabPanel(textOutput("desa2"), 
                                          actionButton("openFormProfilDesa2", "Perbarui Profil Desa", style = "margin-bottom: 10px;display:none;"),
                                          h2(tags$b(textOutput("selamatDatang2"))),
                                          fluidRow(
                                            box(
                                              solidHeader = TRUE,
                                              status = "warning",
                                              h3(tags$b("Sejarah Desa")),
                                              h4(tags$p(textOutput("sejarahDesa2")))
                                            ),
                                            box(
                                              solidHeader = TRUE,
                                              status = "danger",
                                              h3(tags$b("Batas Wilayah Desa")),
                                              h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur2"))),
                                              h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat2"))),
                                              h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan2"))),
                                              h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara2"))),
                                            )
                                          ),
                                          fluidRow(box(
                                            title = h3(tags$b("Persebaran Laki-laki dan Perempuan")),
                                            solidHeader = TRUE,
                                            width = 12,
                                            box(
                                              solidHeader = TRUE,
                                              width = 4,
                                              h4(
                                                tags$b("Total Kepala Keluarga : ") ,
                                                textOutput("kepalaKeluarga2")
                                              )
                                            ),
                                            fluidRow(
                                              box(
                                                solidHeader = TRUE,
                                                width = 8,
                                                plotOutput("lakiPerempuan2")
                                              ),
                                              box(
                                                solidHeader = TRUE,
                                                width = 4,
                                                h4(tags$b("Kepala Desa Saat Ini : ")),
                                                uiOutput("fotoKepalaDesa2"),
                                                div(h4(tags$b(textOutput("kepalaDesa2"))), style = "text-align: center;"),
                                                
                                              )
                                            )
                                          ))
                                 ),
                                 # Tab Ketiga
                                 tabPanel(textOutput("desa3"), 
                                          actionButton("openFormProfilDesa3", "Perbarui Profil Desa", style = "margin-bottom: 10px;display:none;"),
                                          h2(tags$b(textOutput("selamatDatang3"))),
                                          fluidRow(
                                            box(
                                              solidHeader = TRUE,
                                              status = "warning",
                                              h3(tags$b("Sejarah Desa")),
                                              h4(tags$p(textOutput("sejarahDesa3")))
                                            ),
                                            box(
                                              solidHeader = TRUE,
                                              status = "danger",
                                              h3(tags$b("Batas Wilayah Desa")),
                                              h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur3"))),
                                              h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat3"))),
                                              h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan3"))),
                                              h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara3"))),
                                            )
                                          ),
                                          fluidRow(box(
                                            title = h3(tags$b("Persebaran Laki-laki dan Perempuan")),
                                            solidHeader = TRUE,
                                            width = 12,
                                            box(
                                              solidHeader = TRUE,
                                              width = 4,
                                              h4(
                                                tags$b("Total Kepala Keluarga : ") ,
                                                textOutput("kepalaKeluarga3")
                                              )
                                            ),
                                            fluidRow(
                                              box(
                                                solidHeader = TRUE,
                                                width = 8,
                                                plotOutput("lakiPerempuan3")
                                              ),
                                              box(
                                                solidHeader = TRUE,
                                                width = 4,
                                                h4(tags$b("Kepala Desa Saat Ini : ")),
                                                uiOutput("fotoKepalaDesa3"),
                                                div(h4(tags$b(textOutput("kepalaDesa3"))), style = "text-align: center;"),
                                                
                                              )
                                            )
                                          ))
                                          
                                 ),
                                 tabPanel(textOutput("desa4"), 
                                          actionButton("openFormProfilDesa4", "Perbarui Profil Desa", style = "margin-bottom: 10px;display:none;"),
                                          h2(tags$b(textOutput("selamatDatang4"))),
                                          fluidRow(
                                            box(
                                              solidHeader = TRUE,
                                              status = "warning",
                                              h3(tags$b("Sejarah Desa")),
                                              h4(tags$p(textOutput("sejarahDesa4")))
                                            ),
                                            box(
                                              solidHeader = TRUE,
                                              status = "danger",
                                              h3(tags$b("Batas Wilayah Desa")),
                                              h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur4"))),
                                              h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat4"))),
                                              h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan4"))),
                                              h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara4"))),
                                            )
                                          ),
                                          fluidRow(box(
                                            title = h3(tags$b("Persebaran Laki-laki dan Perempuan")),
                                            solidHeader = TRUE,
                                            width = 12,
                                            box(
                                              solidHeader = TRUE,
                                              width = 4,
                                              h4(
                                                tags$b("Total Kepala Keluarga : ") ,
                                                textOutput("kepalaKeluarga4")
                                              )
                                            ),
                                            fluidRow(
                                              box(
                                                solidHeader = TRUE,
                                                width = 8,
                                                plotOutput("lakiPerempuan4")
                                              ),
                                              box(
                                                solidHeader = TRUE,
                                                width = 4,
                                                h4(tags$b("Kepala Desa Saat Ini : ")),
                                                uiOutput("fotoKepalaDesa4"),
                                                div(h4(tags$b(textOutput("kepalaDesa4"))), style = "text-align: center;"),
                                                
                                              )
                                            )
                                          ))
                                 ),
                                 tabPanel(textOutput("desa5"), 
                                          actionButton("openFormProfilDesa5", "Perbarui Profil Desa", style = "margin-bottom: 10px;display:none;"),
                                          h2(tags$b(textOutput("selamatDatang5"))),
                                          fluidRow(
                                            box(
                                              solidHeader = TRUE,
                                              status = "warning",
                                              h3(tags$b("Sejarah Desa")),
                                              h4(tags$p(textOutput("sejarahDesa5")))
                                            ),
                                            box(
                                              solidHeader = TRUE,
                                              status = "danger",
                                              h3(tags$b("Batas Wilayah Desa")),
                                              h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur5"))),
                                              h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat5"))),
                                              h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan5"))),
                                              h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara5"))),
                                            )
                                          ),
                                          fluidRow(box(
                                            title = h3(tags$b("Persebaran Laki-laki dan Perempuan")),
                                            solidHeader = TRUE,
                                            width = 12,
                                            box(
                                              solidHeader = TRUE,
                                              width = 4,
                                              h4(
                                                tags$b("Total Kepala Keluarga : ") ,
                                                textOutput("kepalaKeluarga5")
                                              )
                                            ),
                                            fluidRow(
                                              box(
                                                solidHeader = TRUE,
                                                width = 8,
                                                plotOutput("lakiPerempuan5")
                                              ),
                                              box(
                                                solidHeader = TRUE,
                                                width = 4,
                                                h4(tags$b("Kepala Desa Saat Ini : ")),
                                                uiOutput("fotoKepalaDesa5"),
                                                div(h4(tags$b(textOutput("kepalaDesa5"))), style = "text-align: center;"),
                                                
                                              )
                                            )
                                          ))
                                 ),
                                 tabPanel(textOutput("desa6"), 
                                          actionButton("openFormProfilDesa6", "Perbarui Profil Desa", style = "margin-bottom: 10px;display:none;"),
                                          h2(tags$b(textOutput("selamatDatang6"))),
                                          fluidRow(
                                            box(
                                              solidHeader = TRUE,
                                              status = "warning",
                                              h3(tags$b("Sejarah Desa")),
                                              h4(tags$p(textOutput("sejarahDesa6")))
                                            ),
                                            box(
                                              solidHeader = TRUE,
                                              status = "danger",
                                              h3(tags$b("Batas Wilayah Desa")),
                                              h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur6"))),
                                              h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat6"))),
                                              h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan6"))),
                                              h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara6"))),
                                            )
                                          ),
                                          fluidRow(box(
                                            title = h3(tags$b("Persebaran Laki-laki dan Perempuan")),
                                            solidHeader = TRUE,
                                            width = 12,
                                            box(
                                              solidHeader = TRUE,
                                              width = 4,
                                              h4(
                                                tags$b("Total Kepala Keluarga : ") ,
                                                textOutput("kepalaKeluarga6")
                                              )
                                            ),
                                            fluidRow(
                                              box(
                                                solidHeader = TRUE,
                                                width = 8,
                                                plotOutput("lakiPerempuan6")
                                              ),
                                              box(
                                                solidHeader = TRUE,
                                                width = 4,
                                                h4(tags$b("Kepala Desa Saat Ini : ")),
                                                uiOutput("fotoKepalaDesa6"),
                                                div(h4(tags$b(textOutput("kepalaDesa6"))), style = "text-align: center;"),
                                                
                                              )
                                            )
                                          ))
                                 )
                               ),
                             ), 
                             ),
                             )