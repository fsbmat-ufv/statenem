#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##################################################################
    ######################### DADOS DO PARTICIPANTE ##################
    ##################################################################
    output$table1 <- data.table::renderDataTable({
        data.table::datatable(dados,
                      class = 'cell-border stripe',
                      extensions = 'Buttons', options = list(
                          dom = 'Bfrtip',
                          buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                      ))
    })


    #########
    # Idade #

        output$idade <- renderPlotly({

            df <- dados %>% select(NU_IDADE, TP_SEXO) %>% dplyr::filter(!is.na(NU_IDADE)) %>%
                dplyr::group_by(NU_IDADE, TP_SEXO=as.factor(TP_SEXO)) %>% dplyr::summarise(Valor=n())
            df$TP_SEXO <- plyr::revalue(df$TP_SEXO, c("F"="Feminino", "M"="Masculino"))
        p <-  ggplot2::ggplot2::ggplot(df, aes(NU_IDADE, Valor,
                       group=TP_SEXO, fill=TP_SEXO,
                       text=paste("Idade: ", NU_IDADE, "<br>",
                                  "Sexo: ", TP_SEXO, "<br>",
                                  "Quantidade: ", Valor))) +
            geom_bar(aes(NU_IDADE,Valor,group=TP_SEXO,fill=TP_SEXO), stat = "identity",subset(df,df$TP_SEXO=="Feminino")) +
            geom_bar(aes(NU_IDADE,-Valor,group=TP_SEXO,fill=TP_SEXO), stat = "identity",subset(df,df$TP_SEXO=="Masculino")) +
            scale_y_continuous(breaks=seq(-300000,400000,100000),labels=abs(seq(-300000,400000,100000))) +
            coord_flip()+ggplot2::labs(y="Quantidade de Candidatos", x="Idade")
        plotly::ggplotly(p, tooltip = "text") %>% plotly::layout(legend = list(x = 0.6, y = 0.9))


    })



        output$tabidade <- renderTable({
            tidade <- data.frame(" "=c(" "),
                                  "Idade_Por_Gênero"=c("O gráfico acima apresenta a quantidade de candidatos ao ENEM 2018 por gênero e idade."))
            tidade
        })






    ##########
    # Gênero #

    output$GE <- renderPlotly({
      if(input$variableGE=="GE"){
        df <- dados %>% dplyr::filter(!is.na(TP_SEXO)) %>% dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Numero=n())
        df <- data.frame(GE=c("Feminino", "Masculino"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot2::ggplot(aes(reorder(GE, GE),
                                Quant, fill=GE,
                                label=Quant,
                                text=paste("Gênero: ", GE, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Gênero",
               y="Quantidade",
               title = "Quantidade de Candidatos por Gênero")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(GE=c("Feminino", "Masculino"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot2::ggplot(aes(reorder(GE, Media), Media,
                                    fill=GE, label=Media,
                                    text=paste("Gênero: ", GE, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = GE, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Gênero",
               y="Média de Idade",
               title = "Média de Idade por Gênero")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })


    ################
    # Estado Civil #

    output$EC <- renderPlotly({
      if(input$variableEC=="EC"){
        df <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)) %>% dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Numero=n())
        df <- data.frame(EC=c("0-Solteiro", "1-Casado", "2-Divorciado", "3-Viúvo"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot2::ggplot(aes(reorder(EC, EC),
                                Quant, fill=EC,
                                label=Quant,
                                text=paste("Estado Cívil: ", EC, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Estado Cívil",
               y="Quantidade",
               title = "Quantidade de Candidatos por Estado Cívil")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        names(dados3)<-c("EC","Media","Quant")
        dados3 <- data.frame(EC=c("0-Solteiro", "1-Casado", "2-Divorciado", "3-Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot2::ggplot(aes(reorder(EC, Media), Media,
                                    fill=EC, label=Media,
                                    text=paste("Estado Cívil: ", EC, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Estado Cívil",
               y="Média de Idade",
               title = "Média de Idade por Estado Cívil")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })

    ############
    # Cor/Raça #

    output$Raca <- renderPlotly({
      if(input$variableCor=="Raca1"){
        df <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)) %>% dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Numero=n())
        names(dados)<-c("Raca","Media","Quant")
        df <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot2::ggplot(aes(reorder(Raca, Raca),
                                Quant, fill=Raca,
                                label=Quant,
                                text=paste("Cor/Raça: ", Raca, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Cor/Raça",
               y="Quantidade",
               title = "Quantidade de Candidatos por Cor/Raça")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        names(dados3)<-c("Raca","Media","Quant")
        dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                    fill=Raca, label=Media,
                                    text=paste("Cor/Raça: ", Raca, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Cor/Raça",
               y="Média de Idade",
               title = "Média de Idade por Cor/Raça")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })

    ########################
    # Unidade de Federação #

    output$UF1 <- renderPlotly({
        if(input$variableUF=="UF2"){
            g4 <- dados %>%
                dplyr::group_by(SG_UF_RESIDENCIA) %>% subset(!is.na(SG_UF_RESIDENCIA))  %>%
                dplyr::summarise(quantidade = n()) %>%
                ggplot2::ggplot2::ggplot(aes(x = SG_UF_RESIDENCIA, y = quantidade,
                           fill = SG_UF_RESIDENCIA,
                           text = paste("Estado: ", SG_UF_RESIDENCIA, "<br>",
                                        "Nr. de Candidatos: ", quantidade))) +
                geom_bar(stat = "identity", show.legend = FALSE) +
                ggplot2::geom_text(vjust = -2, size = 3, aes(label=quantidade))+
                #facet_grid(ANO ~ ., scales = "free_y") +
                ggplot2::labs(title = "Quantidade de Candidatos por Estado",
                     x = "Estado", y = "Quantidade")

            plotly::ggplotly(g4, tooltip = "text")%>% style(textposition = "top") %>%
                plotly::layout(showlegend = FALSE)
        }else{
            if(input$variableUF=="UF3"){
                table_UF <- dados %>%
                    select(SG_UF_RESIDENCIA) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>%
                    dplyr::summarise(NUM_CAND = n())
                names(table_UF) <- c("UF_EXERCICIO", "NUM_CAND")

                table_UF <- merge(table_UF, pop, by="UF_EXERCICIO")
                table_UF$PROP = round(1000*(table_UF$NUM_CAND/table_UF$POPULACAO),2)

                ggplot2::ggplot2::ggplot(data=table_UF, aes(x=reorder(UF_EXERCICIO, PROP), y=PROP, fill=REGIAO)) +
                    geom_bar(stat="identity") + coord_flip() +
                    ggplot2::geom_text(hjust = -0.2, size = 5, aes(label=PROP))+
                    ggplot2::labs(title="Número de Candidatos a Cada 1000 Habitantes do Estado", x="Estado",
                         y="Número de Candidatos") +
                    theme_stata()
            }else{
                ##
            }
        }
    })



    ################
    # Ensino Médio #
    ################
    #SITUAÇÃO
    output$Sit <- renderPlotly({
      if(input$variableSit=="Sit1"){
        df <- dados %>% dplyr::filter(!is.na(TP_ST_CONCLUSAO)) %>% dplyr::group_by(TP_ST_CONCLUSAO) %>% dplyr::summarise(Numero=n())
        df <- data.frame(Sit=c("1-Concluído", "2-Concluíndo em 2018", "3-Concluíndo após 2018", "4-Não concluído e não cursando"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot2::ggplot(aes(reorder(Sit, Sit),
                                Quant, fill=Sit,
                                label=Quant,
                                text=paste("Cor/Raça: ", Sit, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Situação de Conclusão do Ensino Médio",
               y="Quantidade",
               title = "Quantidade de Candidatos por Situação de Conclusão")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_ST_CONCLUSAO)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_ST_CONCLUSAO) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(Sit=c("1-Concluído", "2-Concluíndo em 2018", "3-Concluíndo após 2018", "4-Não concluído e não cursando"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot2::ggplot(aes(reorder(Sit, Sit), Media,
                                    fill=Sit, label=Media,
                                    text=paste("Cor/Raça: ", Sit, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Sit, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Situação de Conclusão do Ensino Médio",
               y="Média de Idade",
               title = "Média de Idade por Situação de Conclusão")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })



    ################
    #ANO DE CONCLUSÃO
    output$Anno <- renderPlotly({
      if(input$variableAnno=="Anno1"){
        df <- dados %>% dplyr::filter(!is.na(TP_ANO_CONCLUIU)) %>% dplyr::group_by(TP_ANO_CONCLUIU) %>% dplyr::summarise(Numero=n())
        df <- data.frame(Anno=c("Não informado", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006 ou Antes"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot2::ggplot(aes(reorder(Anno, Anno),
                                Quant, fill=Anno,
                                label=Quant,
                                text=paste("Cor/Raça: ", Anno, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Ano de Conclusão do Ensino Médio",
               y="Quantidade",
               title = "Quantidade de Candidatos por Ano de Conclusão do Ensino Médio")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_ANO_CONCLUIU)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_ANO_CONCLUIU) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(Anno=c("Não informado", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006 ou Antes"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot2::ggplot(aes(reorder(Anno, Anno), Media,
                                    fill=Anno, label=Media,
                                    text=paste("Cor/Raça: ", Anno, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Anno, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Ano de Conclusão do Ensino Médio",
               y="Média de Idade",
               title = "Média de Idade por Ano de Conclusão do Ensino Médio")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })


    ################
    #TIPO DE ESCOLA
    output$Tipo <- renderPlotly({
      if(input$variableTipo=="Tipo1"){
        df <- dados %>% dplyr::filter(!is.na(TP_ESCOLA)) %>% dplyr::group_by(TP_ESCOLA) %>% dplyr::summarise(Numero=n())
        df <- data.frame(Tipo=c("Não Respondeu", "Pública", "Exterior", "Privada"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot2::ggplot(aes(reorder(Tipo, Tipo),
                                Quant, fill=Tipo,
                                label=Quant,
                                text=paste("Cor/Raça: ", Tipo, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Tipo de Escola",
               y="Quantidade",
               title = "Quantidade de Candidatos por Tipo de Escola")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_ESCOLA)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_ESCOLA) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(Tipo=c("Não Respondeu", "Pública", "Exterior", "Privada"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot2::ggplot(aes(reorder(Tipo, Tipo), Media,
                                    fill=Tipo, label=Media,
                                    text=paste("Cor/Raça: ", Tipo, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Tipo, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Tipo de Escola",
               y="Média de Idade",
               title = "Média de Idade por Tipo de Escola")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })




    ################
    #TIPO DE ENSINO
    output$Tip <- renderPlotly({
      if(input$variableTip=="Tip1"){
        df <- dados %>% dplyr::filter(!is.na(TP_ENSINO)) %>% dplyr::group_by(TP_ENSINO) %>% dplyr::summarise(Numero=n())
        df <- data.frame(Tip=c("Ensino Regular", "Educação Especial - Modalidade Substitutiva", "Educação de Jovens e Adultos"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot(aes(reorder(Tip, Tip),
                                Quant, fill=Tip,
                                label=Quant,
                                text=paste("Cor/Raça: ", Tip, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Tipo de Ensino",
               y="Quantidade",
               title = "Quantidade de Candidatos por Tipo de Ensino")
        plotly::plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_ENSINO)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_ENSINO) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(Tip=c("Ensino Regular", "Educação Especial - Modalidade Substitutiva", "Educação de Jovens e Adultos"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Tip, Tip), Media,
                                    fill=Tip, label=Media,
                                    text=paste("Cor/Raça: ", Tip, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Tip, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Tipo de Ensino",
               y="Média de Idade",
               title = "Média de Idade por Tipo de Ensino")
        plotly::plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })




    ###################
    # Dados da Escola #



    #####################################
    #ESFERA - DEPENDÊNCIA ADMINISTRATIVA

    output$Dadm <- renderPlotly({
        if(input$variableDadm=="Dadm1"){
          df <- dados %>% dplyr::filter(!is.na(TP_DEPENDENCIA_ADM_ESC)) %>% dplyr::group_by(TP_DEPENDENCIA_ADM_ESC) %>% dplyr::summarise(Numero=n())
          df <- data.frame(Dadm=c("Federal", "Estadual", "Municipal", "Privada"), Quant=df$Numero)
          df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
          p1 <- df %>% ggplot2::ggplot(aes(reorder(Dadm, Dadm),
                                  Quant, fill=Dadm,
                                  label=Quant,
                                  text=paste("Cor/Raça: ", Dadm, "<br>",
                                             "Número de Candidatos: ", Quant, "<br>",
                                             "Proporção Candidatos: ", Prop))) +
            ggplot2::ggplot2::geom_col(show.legend = FALSE)+
            ggplot2::geom_text()+
            ggplot2::labs(x="Esfera Administrativa",
                 y="Quantidade",
                 title = "Quantidade de Candidatos por Esfera Administrativa")
          plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
            style(textposition = "top")

        } else {
          dados3 <- dados %>% dplyr::filter(!is.na(TP_DEPENDENCIA_ADM_ESC)&!is.na(NU_IDADE)) %>%
            dplyr::group_by(TP_DEPENDENCIA_ADM_ESC) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
          dados3 <- data.frame(Dadm=c("Federal", "Estadual", "Municipal", "Privada"), Media=dados3$Media, Quant=dados3$Quant)
          p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Dadm, Dadm), Media,
                                      fill=Dadm, label=Media,
                                      text=paste("Cor/Raça: ", Dadm, "<br>",
                                                 "Média de Idade: ", Media, "<br>",
                                                 "Quantidade: ", Quant))) +
            ggplot2::geom_col(show.legend = FALSE)+
            ggplot2::geom_text(aes(x = Dadm, y = round(Media, digits = 2),
                          label = paste0(round(Media, digits = 2)," Anos")), size=4)+
            ggplot2::labs(x="Esfera Administrativa",
                 y="Média de Idade",
                 title = "Média de Idade por Esfera Administrativa")
          plotly::ggplotly(p1, tooltip = "text") %>% plotly::plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

        }

      })

    #############
    #LOCALIZAÇÃO
    output$Loc <- renderPlotly({
      if(input$variableLoc=="Loc1"){
        df <- dados %>% dplyr::filter(!is.na(TP_LOCALIZACAO_ESC)) %>% dplyr::group_by(TP_LOCALIZACAO_ESC) %>% dplyr::summarise(Numero=n())
        df <- data.frame(Loc=c("Urbana", "Rural"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot(aes(reorder(Loc, Loc),
                                Quant, fill=Loc,
                                label=Quant,
                                text=paste("Cor/Raça: ", Loc, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Localização",
               y="Quantidade",
               title = "Quantidade de Candidatos por Localização")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_LOCALIZACAO_ESC)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_LOCALIZACAO_ESC) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(Loc=c("Urbana", "Rural"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Loc, Loc), Media,
                                    fill=Loc, label=Media,
                                    text=paste("Cor/Raça: ", Loc, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Loc, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Localização",
               y="Média de Idade",
               title = "Média de Idade por Localização")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })
    ##################################################################





    ##############
    #TREEMAP
    output$treemap1<- renderPlot({

        aggSetor <-dados %>%
            dplyr::group_by(SG_UF_RESIDENCIA) %>%
            dplyr::dplyr::summarise(quantidade = n(), notaMedia = mean(nt_final,na.rm = TRUE))
        aggSetor$escala <- scale(aggSetor$notaMedia) #necessário para criar valores negativos para deixar as disparidades mais evidentes
        treemap::treemap(aggSetor, index = "SG_UF_RESIDENCIA", vSize = "quantidade", vColor = "escala",
                type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
                title  =  "Treemap da Média das Notas do Enem por Estado")
    })

        output$tabtreemap <- renderTable({
          ttreemap <- data.frame(" "=c(" "),
                               "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          ttreemap


        })




    #################
    # Nacionalidade #

    output$Nac <- renderPlotly({
      if(input$variableNac=="Nac1"){
        df <- dados %>% dplyr::filter(!is.na(TP_NACIONALIDADE)) %>% dplyr::group_by(TP_NACIONALIDADE) %>% dplyr::dplyr::summarise(Numero=n())
        #names(dados)<-c("Nac","Media","Quant")
        df <- data.frame(Nac=c("0-Não informado", "1-Brasileiro Nato", "2-Naturarizado", "3-Estrangeiro", "4-Nato, nascido no exterior"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot(aes(reorder(Nac, Nac),
                                Quant, fill=Nac,
                                label=Quant,
                                text=paste("Cor/Raça: ", Nac, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Nacionalidade",
               y="Quantidade",
               title = "Quantidade de Candidatos por Nacionalidade")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(TP_NACIONALIDADE)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(TP_NACIONALIDADE) %>% dplyr::dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(Nac=c("0-Não informado", "1-Brasileiro Nato", "2-Naturarizado", "3-Estrangeiro", "4-Nato, nascido no exterior"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Nac, Nac), Media,
                                    fill=Nac, label=Media,
                                    text=paste("Cor/Raça: ", Nac, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Nac, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Nacionalidade",
               y="Média de Idade",
               title = "Média de Idade por Nacionalidade")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })





    ################
    # Treineiro

    output$Trei <- renderPlotly({
      if(input$variableTrei=="Trei1"){
        df <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)) %>% dplyr::group_by(IN_TREINEIRO) %>% dplyr::dplyr::summarise(Numero=n())
        df <- data.frame(Trei=c("Não Treineiro", "Treineiro"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot(aes(reorder(Trei, Quant, sum),
                                Quant, fill=Trei,
                                label=Quant,
                                text=paste("Cor/Raça: ", Trei, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Treineiro",
               y="Quantidade",
               title = "Quantidade de Candidatos em Condição de Treinamento")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")

      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_IDADE)) %>%
          dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_IDADE),Quant=n())
        dados3 <- data.frame(Trei=c("Não Treineiro", "Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Trei, Media), Media,
                                    fill=Trei, label=Media,
                                    text=paste("Cor/Raça: ", Trei, "<br>",
                                               "Média de Idade: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Trei, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," Anos")), size=4)+
          ggplot2::labs(x="Treineiro",
               y="Média de Idade",
               title = "Média de Idade dos Candidatos em Condição de Treinamento")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }

    })


    ######################### DESEMPENHO #############################
    ##################################################################

    output$D <- renderPlotly({

      if(input$variableD=="D1"){
        dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(nt_final)) %>%
          dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        dados3 <- data.frame(Raca=c("ND", "Branca", "Preta", "Parda", "Amarela", "Indígena"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Media), Media,
                                    fill=Raca, label=Media,
                                    text=paste("Cor/Raça: ", Raca, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Cor/Raça",
               y="Nota Média",
               title = "Nota Média por Cor/Raça")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else {
        if(input$variableD=="D2"){
          dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(nt_final)) %>%
            dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
          dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
          p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Media), Media,
                                      fill=Sexo, label=Media,
                                      text=paste("Gênero: ", Sexo, "<br>",
                                                 "Nota Média: ", Media, "<br>",
                                                 "Quantidade: ", Quant))) +
            ggplot2::geom_col(show.legend = FALSE)+
            ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                          label = paste0(round(Media, digits = 2)," ")), size=4)+
            ggplot2::labs(x="Gênero",
                 y="Nota Média",
                 title = "Nota Média por Gênero")
          plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

        }
        else {
          if(input$variableD=="D3"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(nt_final)) %>%
              dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
            dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, Media), Media,
                                        fill=EC, label=Media,
                                        text=paste("Estado Civil: ", EC, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Estado Civil",
                   y="Nota Média",
                   title = "Nota Média por Estado Civil")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          }
          else {
            if(input$variableD=="D4"){
            dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(nt_final)) %>%
              dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
            dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                        fill=UF, label=Media,
                                        text=paste("Estado: ", UF, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Estado",
                   y="Nota Média",
                   title = "Nota Média por Estado")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else{
            if(input$variableD=="D5"){
              dados3 <- dados %>% dplyr::filter(!is.na(Q002)&!is.na(nt_final)) %>%
                dplyr::group_by(Q002) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
              dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(UF, Media,
                                          fill=UF, label=Media,
                                          text=paste("Estado: ", UF, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Escolaridade da Mãe",
                     y="Nota Média",
                     title = "Nota Média por Escolaridade da Mãe")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            } else{
              if(input$variableD=="D6"){
                dados3 <- dados %>% dplyr::filter(!is.na(Q001)&!is.na(nt_final)) %>%
                  dplyr::group_by(Q001) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                            fill=UF, label=Media,
                                            text=paste("Estado: ", UF, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Escolaridade do Pai",
                       y="Nota Média",
                       title = "Nota Média por Escolaridade do Pai")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

              } else{
                if(input$variableD=="D7"){
                  dados3 <- dados %>% dplyr::filter(!is.na(Q004)&!is.na(nt_final)) %>%
                    dplyr::group_by(Q004) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                  dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(UF, Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                  label = paste0(round(Media, digits = 2)," ")), size=4)+
                    ggplot2::labs(x="Ocupação da Mãe",
                         y="Nota Média",
                         title = "Nota Média por Ocupação da Mãe")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                } else{
                  if(input$variableD=="D8"){
                    dados3 <- dados %>% dplyr::filter(!is.na(Q003)&!is.na(nt_final)) %>%
                      dplyr::group_by(Q003) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                    dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                                fill=UF, label=Media,
                                                text=paste("Estado: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Ocupação do Pai",
                           y="Nota Média",
                           title = "Nota Média por Ocupação do Pai")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                   } else{
                    if(input$variableD=="D9"){
                      dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(nt_final)) %>%
                        dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                      dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                      p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                                  fill=UF, label=Media,
                                                  text=paste("Estado: ", UF, "<br>",
                                                             "Nota Média: ", Media, "<br>",
                                                             "Quantidade: ", Quant))) +
                        ggplot2::geom_col(show.legend = FALSE)+
                        ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                      label = paste0(round(Media, digits = 2)," ")), size=4)+
                        ggplot2::labs(x="Tipo de Escola",
                             y="Nota Média",
                             title = "Nota Média por Tipo de Escola")
                      plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")



                  } else{
                    if(input$variableD=="D10"){
                    dados3 <- dados %>% dplyr::filter(!is.na(Q006)&!is.na(nt_final)) %>%
                      dplyr::group_by(Q006) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                    dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                                fill=UF, label=Media,
                                                text=paste("Estado: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Renda",
                           y="Nota Média",
                           title = "Nota Média por Renda da Família")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                   } else{
                        dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(nt_final)) %>%
                          dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                        dados3 <- data.frame(UF=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                                    fill=UF, label=Media,
                                                    text=paste("Tipo de Candidato: ", UF, "<br>",
                                                               "Nota Média: ", Media, "<br>",
                                                               "Quantidade: ", Quant))) +
                          ggplot2::geom_col(show.legend = FALSE)+
                          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                        label = paste0(round(Media, digits = 2)," ")), size=4)+
                          ggplot2::labs(x="Tipo de Candidato",
                               y="Nota Média",
                               title = "Nota Média por Tipo de Candidato")
                        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")
                   }
                  }
                }
              }
            }
          }
        }
       }
      }
     }
   })






    ######################### DETALHAMENTO DAS PROVAS ################
    ##################################################################

    ########################
    # Ciências da Natureza #
    ########################

    output$CN <- renderPlotly({

      if(input$variableCN=="CN1"){
        dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_CN)) %>%
          dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_CN),Quant=n())
        names(dados3)<-c("Raca","Media","Quant")
        dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                    fill=Raca, label=Media,
                                    text=paste("Cor/Raça: ", Raca, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Cor/Raça",
               y="Nota Média",
               title = "Nota Média por Cor/Raça em Ciências da Natureza")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else {
        if(input$variableCN=="CN2"){
        dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_CN)) %>%
          dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_CN),Quant=n())
        names(dados3)<-c("Sexo","Media","Quant")
        dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                    fill=Sexo, label=Media,
                                    text=paste("Gênero: ", Sexo, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Gênero",
               y="Nota Média",
               title = "Nota Média por Gênero em Ciências da Natureza")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      }
      else {
        if(input$variableCN=="CN3"){
        dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_CN)) %>%
          dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_CN),Quant=n())
        names(dados3)<-c("EC","Media","Quant")
        dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                    fill=EC, label=Media,
                                    text=paste("Estado Civil: ", EC, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Estado Civil",
               y="Nota Média",
               title = "Nota Média por Estado Civil em Ciências da Natureza")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


        } else {
          if(input$variableCN=="CN4"){
            dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_CN)) %>%
              dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_CN),Quant=n())
            names(dados3)<-c("UF","Media","Quant")
            #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                        fill=UF, label=Media,
                                        text=paste("Estado: ", UF, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                            label = paste0(round(Media, digits = 1)," ")), size=2)+
              ggplot2::labs(x="Estado",
                   y="Nota Média",
                   title = "Nota Média por Estado em Ciências da Natureza")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          }
          else {
            if(input$variableCN=="CN5"){
              dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_CN)) %>%
                dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_CN),Quant=n())
              names(dados3)<-c("IN_TREINEIRO","Media","Quant")
              dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                          fill=IN_TREINEIRO, label=Media,
                                          text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Tipo de Candidato",
                     y="Nota Média",
                     title = "Nota Média por Tipo de Candidato em Ciências da Natureza")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




      } else {
        dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_CN)) %>%
          dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_CN),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Tipo de Escola: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Tipo de Escola",
               y="Nota Média",
               title = "Nota Média por Tipo de Escola em Ciências da Natureza")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


      }
      }
      }
      }
      }


    })

        output$CNQ027 <- renderTable({
#          ttreemap1 <- data.frame(" "=c(" "),
#                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
#          ttreemap1


          if(input$variableCN=="CN6") {
            CNQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                  "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                  "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                  "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            CNQ027

          }
                })


    ####################
    # Ciências Humanas #
    ####################

        output$CH <- renderPlotly({

          if(input$variableCH=="CH1"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_CH)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_CH),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça em Ciências Humanas")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableCH=="CH2"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_CH)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_CH),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero em Ciências Humanas")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableCH=="CH3"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_CH)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_CH),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil em Ciências Humanas")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableCH=="CH4"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_CH)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_CH),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado em Ciências Humanas")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableCH=="CH5"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_CH)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_CH),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato em Ciências Humanas")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_CH)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_CH),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola em Ciências Humanas")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$CHQ027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableCH=="CH6") {
            CHQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            CHQ027

          }
        })


        ####################
    # Linguagens e Códigos #
    ########################

        output$LC <- renderPlotly({

          if(input$variableLC=="LC1"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_LC)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_LC),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça em Linguagens e Códigos")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableLC=="LC2"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_LC)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_LC),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero em Linguagens e Códigos")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableLC=="LC3"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_LC)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_LC),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil em Linguagens e Códigos")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableLC=="LC4"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_LC)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_LC),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado em Linguagens e Códigos")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableLC=="LC5"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_LC)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_LC),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato em Linguagens e Códigos")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_LC)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_LC),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola em Linguagens e Códigos")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$LCQ027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableLC=="LC6") {
            LCQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            LCQ027

          }
        })


    ####################
    # Matemática #
    ########################

        output$MT <- renderPlotly({

          if(input$variableMT=="MT1"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_MT)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_MT),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça em Matemática")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableMT=="MT2"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_MT)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_MT),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero em Matemática")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableMT=="MT3"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_MT)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_MT),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil em Matemática")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableMT=="MT4"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_MT)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_MT),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado em Matemática")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableMT=="MT5"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_MT)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_MT),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato em Matemática")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_MT)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_MT),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola em Matemática")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$MTQ027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableMT=="MT6") {
            MTQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            MTQ027

          }
        })


    ####################
    # Nota Final #
    ########################

        output$NF <- renderPlotly({

          if(input$variableNF=="NF1"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(nt_final)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça (Nota Final)")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableNF=="NF2"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(nt_final)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero (Nota Final)")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableNF=="NF3"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(nt_final)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil (Nota Final)")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableNF=="NF4"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(nt_final)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado (Nota Final)")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableNF=="NF5"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(nt_final)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato (Nota Final)")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(nt_final)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola (Nota Final)")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$NFQ027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableNF=="NF6") {
            NFQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            NFQ027

          }
        })


        ####################
    # Redação #

    output$SituationRED <- renderPlotly({


        df <- dados %>% dplyr::filter(!is.na(TP_STATUS_REDACAO)) %>% dplyr::group_by(TP_STATUS_REDACAO) %>% dplyr::summarise(Numero=n())
        df <- data.frame(Raca=c("1", "2", "3", "4", "6", "7", "8", "9"), Quant=df$Numero)
        df$Prop <- round((df$Quant/sum(df$Quant)), digits = 3)
        p1 <- df %>% ggplot2::ggplot(aes(reorder(Raca, Raca),
                                Quant, fill=Raca,
                                label=Quant,
                                text=paste("Situação da Redação: ", Raca, "<br>",
                                           "Número de Candidatos: ", Quant, "<br>",
                                           "Proporção Candidatos: ", Prop))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text()+
          ggplot2::labs(x="Situação da Redação",
               y="Média de Idade",
               title = "Quantidade de Candidatos por Situação da Redação")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE) %>%
          style(textposition = "top")
    })



        output$REDSIT <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


#          if(input$variableCN=="CN6") {
            REDSIT <- data.frame("Resposta"=c("1", "2", "3", "4", "6", "7","8","9"),
                                 "Significado"=c("Sem Problemas","Anulada",
                                                 "Cópia do Texto Motivador","Em Branco",
                                                 "Fuga ao Tema","Não Atendimento ao Tipo Textual",
                                                 "Texto Insuficiente", "Parte desconectada"))
            REDSIT

#          }
        })





    ##############
    #COMPETÊNCIA 1
    ########################

        output$COMP1 <- renderPlotly({

          if(input$variableCOMP1=="COMP11"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_COMP1)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP1),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça na Competência 1 da Redação")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableCOMP1=="COMP12"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_COMP1)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP1),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero na Competência 1 da Redação")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableCOMP1=="COMP13"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_COMP1)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP1),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil na Competência 1 da Redação")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableCOMP1=="COMP14"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_COMP1)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP1),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado na Competência 1 da Redação")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableCOMP1=="COMP15"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_COMP1)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP1),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato na Competência 1 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_COMP1)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP1),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola na Competência 1 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$COMP1Q027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableCOMP1=="COMP16") {
            COMP1Q027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            COMP1Q027

          }
        })


    ####################
    #COMPETÊNCIA 2
        ########################

        output$COMP2 <- renderPlotly({

          if(input$variableCOMP2=="COMP21"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_COMP2)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP2),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça na Competência 2 da Redação")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableCOMP2=="COMP22"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_COMP2)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP2),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero na Competência 2 da Redação")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableCOMP2=="COMP23"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_COMP2)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP2),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil na Competência 2 da Redação")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableCOMP2=="COMP24"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_COMP2)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP2),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado na Competência 2 da Redação")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableCOMP2=="COMP25"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_COMP2)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP2),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato na Competência 2 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_COMP2)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP2),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola na Competência 2 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$COMP2Q027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableCOMP2=="COMP26") {
            COMP2Q027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            COMP2Q027

          }
        })


        ####################
    #COMPETÊNCIA 3
        ########################

        output$COMP3 <- renderPlotly({

          if(input$variableCOMP3=="COMP31"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_COMP3)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP3),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça na Competência 3 da Redação")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableCOMP3=="COMP32"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_COMP3)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP3),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero na Competência 3 da Redação")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableCOMP3=="COMP33"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_COMP3)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP3),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil na Competência 3 da Redação")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableCOMP3=="COMP34"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_COMP3)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP3),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado na Competência 3 da Redação")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableCOMP3=="COMP35"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_COMP3)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP3),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato na Competência 3 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_COMP3)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP3),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola na Competência 3 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$COMP3Q027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableCOMP3=="COMP36") {
            COMP3Q027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            COMP3Q027

          }
        })


        ####################
    #COMPETÊNCIA 4
        ########################

        output$COMP4 <- renderPlotly({

          if(input$variableCOMP4=="COMP41"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_COMP4)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP4),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça na Competência 4 da Redação")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableCOMP4=="COMP42"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_COMP4)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP4),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero na Competência 4 da Redação")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableCOMP4=="COMP43"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_COMP4)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP4),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil na Competência 4 da Redação")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableCOMP4=="COMP44"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_COMP4)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP4),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado na Competência 4 da Redação")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableCOMP4=="COMP45"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_COMP4)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP4),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato na Competência 4 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_COMP4)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP4),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola na Competência 4 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$COMP4Q027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableCOMP4=="COMP46") {
            COMP4Q027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            COMP4Q027

          }
        })


        ####################
    #COMPETÊNCIA 5
        ########################

        output$COMP5 <- renderPlotly({

          if(input$variableCOMP5=="COMP51"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_COMP5)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP5),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça na Competência 5 da Redação")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableCOMP5=="COMP52"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_COMP5)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP5),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero na Competência 5 da Redação")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableCOMP5=="COMP53"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_COMP5)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP5),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil na Competência 5 da Redação")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableCOMP5=="COMP54"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_COMP5)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP5),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado na Competência 5 da Redação")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableCOMP5=="COMP55"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_COMP5)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP5),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato na Competência 5 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_COMP5)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_COMP5),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola na Competência 5 da Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$COMP5Q027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableCOMP5=="COMP56") {
            COMP5Q027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            COMP5Q027

          }
        })


    ####################
    #NOTA FINAL DA REDAÇÃO
    ########################

        output$RE <- renderPlotly({

          if(input$variableRE=="RE1"){
            dados3 <- dados %>% dplyr::filter(!is.na(TP_COR_RACA)&!is.na(NU_NOTA_REDACAO)) %>%
              dplyr::group_by(TP_COR_RACA) %>% dplyr::summarise(Media=mean(NU_NOTA_REDACAO),Quant=n())
            names(dados3)<-c("Raca","Media","Quant")
            dados3 <- data.frame(Raca=c("0-Não Declarado", "1-Branca", "2-Preta", "3-Parda", "4-Amarela", "5-Indígena"), Media=dados3$Media, Quant=dados3$Quant)
            p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Raca, Raca), Media,
                                        fill=Raca, label=Media,
                                        text=paste("Cor/Raça: ", Raca, "<br>",
                                                   "Nota Média: ", Media, "<br>",
                                                   "Quantidade: ", Quant))) +
              ggplot2::geom_col(show.legend = FALSE)+
              ggplot2::geom_text(aes(x = Raca, y = round(Media, digits = 2),
                            label = paste0(round(Media, digits = 2)," ")), size=4)+
              ggplot2::labs(x="Cor/Raça",
                   y="Nota Média",
                   title = "Nota Média por Cor/Raça na Redação")
            plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

          } else {
            if(input$variableRE=="RE2"){
              dados3 <- dados %>% dplyr::filter(!is.na(TP_SEXO)&!is.na(NU_NOTA_REDACAO)) %>%
                dplyr::group_by(TP_SEXO) %>% dplyr::summarise(Media=mean(NU_NOTA_REDACAO),Quant=n())
              names(dados3)<-c("Sexo","Media","Quant")
              dados3 <- data.frame(Sexo=c("Feminino", "Masculino" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(Sexo, Sexo), Media,
                                          fill=Sexo, label=Media,
                                          text=paste("Gênero: ", Sexo, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = Sexo, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Gênero",
                     y="Nota Média",
                     title = "Nota Média por Gênero na Redação")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

            }
            else {
              if(input$variableRE=="RE3"){
                dados3 <- dados %>% dplyr::filter(!is.na(TP_ESTADO_CIVIL)&!is.na(NU_NOTA_REDACAO)) %>%
                  dplyr::group_by(TP_ESTADO_CIVIL) %>% dplyr::summarise(Media=mean(NU_NOTA_REDACAO),Quant=n())
                names(dados3)<-c("EC","Media","Quant")
                dados3 <- data.frame(EC=c("Solteiro", "Casado", "Divorciado", "Viúvo"), Media=dados3$Media, Quant=dados3$Quant)
                p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(EC, EC), Media,
                                            fill=EC, label=Media,
                                            text=paste("Estado Civil: ", EC, "<br>",
                                                       "Nota Média: ", Media, "<br>",
                                                       "Quantidade: ", Quant))) +
                  ggplot2::geom_col(show.legend = FALSE)+
                  ggplot2::geom_text(aes(x = EC, y = round(Media, digits = 2),
                                label = paste0(round(Media, digits = 2)," ")), size=4)+
                  ggplot2::labs(x="Estado Civil",
                       y="Nota Média",
                       title = "Nota Média por Estado Civil na Redação")
                plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


              } else {
                if(input$variableRE=="RE4"){
                  dados3 <- dados %>% dplyr::filter(!is.na(SG_UF_RESIDENCIA)&!is.na(NU_NOTA_REDACAO)) %>%
                    dplyr::group_by(SG_UF_RESIDENCIA) %>% dplyr::summarise(Media=mean(NU_NOTA_REDACAO),Quant=n())
                  names(dados3)<-c("UF","Media","Quant")
                  #dados3 <- data.frame(UF=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27" ), Media=dados3$Media, Quant=dados3$Quant)
                  p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, Media), Media,
                                              fill=UF, label=Media,
                                              text=paste("Estado: ", UF, "<br>",
                                                         "Nota Média: ", Media, "<br>",
                                                         "Quantidade: ", Quant))) +
                    ggplot2::geom_col(show.legend = FALSE)+
                    ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 1),
                                  label = paste0(round(Media, digits = 1)," ")), size=2)+
                    ggplot2::labs(x="Estado",
                         y="Nota Média",
                         title = "Nota Média por Estado na Redação")
                  plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

                }
                else {
                  if(input$variableRE=="RE5"){
                    dados3 <- dados %>% dplyr::filter(!is.na(IN_TREINEIRO)&!is.na(NU_NOTA_REDACAO)) %>%
                      dplyr::group_by(IN_TREINEIRO) %>% dplyr::summarise(Media=mean(NU_NOTA_REDACAO),Quant=n())
                    names(dados3)<-c("IN_TREINEIRO","Media","Quant")
                    dados3 <- data.frame(IN_TREINEIRO=c("Treineiro", "Não Treineiro"), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(IN_TREINEIRO, IN_TREINEIRO), Media,
                                                fill=IN_TREINEIRO, label=Media,
                                                text=paste("Tipo de Candidato: ", IN_TREINEIRO, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = IN_TREINEIRO, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Candidato",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Candidato na Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")




                  } else {
                    dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(NU_NOTA_REDACAO)) %>%
                      dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(NU_NOTA_REDACAO),Quant=n())
                    names(dados3)<-c("UF","Media","Quant")
                    #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F" ), Media=dados3$Media, Quant=dados3$Quant)
                    p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                                fill=UF, label=Media,
                                                text=paste("Tipo de Escola: ", UF, "<br>",
                                                           "Nota Média: ", Media, "<br>",
                                                           "Quantidade: ", Quant))) +
                      ggplot2::geom_col(show.legend = FALSE)+
                      ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                                    label = paste0(round(Media, digits = 2)," ")), size=4)+
                      ggplot2::labs(x="Tipo de Escola",
                           y="Nota Média",
                           title = "Nota Média por Tipo de Escola na Redação")
                    plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


                  }
                }
              }
            }
          }


        })

        output$REQ027 <- renderTable({
          #          ttreemap1 <- data.frame(" "=c(" "),
          #                                 "Treemap"=c("O gráfico acima demonstra o desempenho médio dos candidados por Estado, conforme escala de cores. A área de cada Estado no gráfico é proporcional à quantidade de candidatos."))
          #          ttreemap1


          if(input$variableRE=="RE6") {
            REQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                 "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                 "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                 "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
            REQ027

          }
        })


    ####################
    ##################################################################
    ################ QUESTIONÁRIO SOCIOECONÔMICO #####################
    ##################################################################

    output$TextQ00 <- renderText({
        if(input$variableQ=="Q001") {
            "Até que série seu pai, ou o homem responsável por você, estudou?"
        } else {
            if(input$variableQ=="Q002") {
                "Até que série sua mãe, ou a mulher responsável por você, estudou?"
            } else {
                if(input$variableQ=="Q003") {
                    "indique o grupo que contempla a ocupação mais próxima da ocupação do seu pai ou do homem responsável por você. (Se ele não estiver trabalhando, escolha uma ocupação pensando no último trabalho dele)"
                } else {
                    if(input$variableQ=="Q004") {
                        "indique o grupo que contempla a ocupação mais próxima da ocupação da sua mãe ou da mulher responsável por você. (Se ela não estiver trabalhando, escolha uma ocupação pensando no último trabalho dela)"
                    } else {
                        if(input$variableQ=="Q005") {
                            "Incluindo você, quantas pessoas moram atualmente em sua residência?"
                        } else {
                            if(input$variableQ=="Q006") {
                                "Qual é a renda mensal de sua família, somando a sua renda com a dos seus familiares?"
                            } else {
                                if(input$variableQ=="Q007") {
                                    "Em sua residência trabalha empregado(a) doméstico(a)?"
                                } else {
                                    if(input$variableQ=="Q008") {
                                        "Na sua residência tem banheiro?"
                                    } else {
                                        if(input$variableQ=="Q009") {
                                            "Na sua residência tem quartos para dormir?"
                                        } else {
                                            if(input$variableQ=="Q010") {
                                                "Na sua residência tem carro?"
                                            } else {
                                                if(input$variableQ=="Q011") {
                                                    "Na sua residência tem motocicleta?"
                                                } else {
                                                    if(input$variableQ=="Q012") {
                                                        "Na sua residência tem geladeira?"
                                                    } else {
                                                        if(input$variableQ=="Q013") {
                                                            "Na sua residência tem freezer (independente ou segunda porta da geladeira)?"
                                                        } else {
                                                            if(input$variableQ=="Q014") {
                                                                "Na sua residência tem máquina de lavar roupa?"
                                                            } else {
                                                                if(input$variableQ=="Q015") {
                                                                    "Na sua residência tem máquina de secar roupa (independente ou em conjunto com a máquina de lavar roupa)?"
                                                                } else {
                                                                    if(input$variableQ=="Q016") {
                                                                        "Na sua residência tem forno micro-ondas?"
                                                                    } else {
                                                                        if(input$variableQ=="Q017") {
                                                                            "Na sua residência tem máquina de lavar louça?"
                                                                        } else {
                                                                            if(input$variableQ=="Q018") {
                                                                                "Na sua residência tem aspirador de pó?"
                                                                            } else {
                                                                                if(input$variableQ=="Q019") {
                                                                                    "Na sua residência tem televisão em cores?"
                                                                                } else {
                                                                                    if(input$variableQ=="Q020") {
                                                                                        "Na sua residência tem aparelho de DVD?"
                                                                                    } else {
                                                                                        if(input$variableQ=="Q021") {
                                                                                            "Na sua residência tem TV por assinatura?"
                                                                                        } else {
                                                                                            if(input$variableQ=="Q022") {
                                                                                                "Na sua residência tem telefone celular?"
                                                                                            } else {
                                                                                                if(input$variableQ=="Q023") {
                                                                                                    "Na sua residência tem telefone fixo?"
                                                                                                } else {
                                                                                                    if(input$variableQ=="Q024") {
                                                                                                        "Na sua residência tem computador?"
                                                                                                    } else {
                                                                                                        if(input$variableQ=="Q025") {
                                                                                                            "Na sua residência tem acesso à Internet?"
                                                                                                        } else {
                                                                                                            if(input$variableQ=="Q026") {
                                                                                                                "Você já concluiu ou está concluindo o Ensino Médio?"
                                                                                                            } else {
                                                                                                                if(input$variableQ=="Q027") {
                                                                                                                    "Em que tipo de escola você frequentou o Ensino Médio?"
                                                                                                                } else {
                                                                                                                    ##
                                                                                                                }
                                                                                                            }
                                                                                                        }
                                                                                                    }
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            }
    })


    output$Q00 <- renderPlotly({
        if(input$variableQ=="Q001") {

            temp <- dados %>%
                mutate(Q001 = ifelse(Q001=="", NA, Q001)) %>%
                mutate(Q001 = factor(Q001, labels = unique(dados$Q001) ))
            #temp$Q001 <- factor(temp$Q001, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
            temp$Q001 <- factor(temp$Q001, labels = c("A", "B", "C", "D", "E", "F", "G", "H"))
            temp <- as.data.frame(table(temp$Q001))
            tplot1 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q001", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q001", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q002") {

            temp <- dados %>%
                mutate(Q002 = ifelse(Q002=="", NA, Q002)) %>%
                mutate(Q002 = factor(Q002, labels = unique(dados$Q002) ))
            temp$Q002 <- factor(temp$Q002, labels = c("A", "B", "C", "D", "E", "F", "G", "H"))
            temp <- as.data.frame(table(temp$Q002))
            tplot2 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q002", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q002", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot2, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q003") {

            temp <- dados %>%
                mutate(Q003 = ifelse(Q003=="", NA, Q003)) %>%
                mutate(Q003 = factor(Q003, labels = unique(dados$Q003) ))
            temp$Q003 <- factor(temp$Q003, labels = c("A", "B", "C", "D", "E", "F"))
            temp <- as.data.frame(table(temp$Q003))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q003", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q003", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q004") {
            temp <- dados %>%
                mutate(Q004 = ifelse(Q004=="", NA, Q004)) %>%
                mutate(Q004 = factor(Q004, labels = unique(dados$Q004) ))
            temp$Q004 <- factor(temp$Q004, labels = c("A", "B", "C", "D", "E", "F"))
            temp <- as.data.frame(table(temp$Q004))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q004", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q004", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")


        } else{ if(input$variableQ=="Q005") {
            temp <- dados %>%
                mutate(Q005 = ifelse(Q005=="", NA, Q005)) %>%
                mutate(Q005 = factor(Q005, labels = unique(dados$Q005) ))
            temp$Q005 <- factor(temp$Q005, labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"))
            temp <- as.data.frame(table(temp$Q005))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q005", values=c ("#005fff","#007fff","#009fff","#00afff","#00cfff","#00efff","#00ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff")) +
#                scale_fill_manual("Q005", values=brewer.pal (9, "color(20)")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q005", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q006") {
            temp <- dados %>%
                mutate(Q006 = ifelse(Q006=="", NA, Q006)) %>%
                mutate(Q006 = factor(Q006, labels = unique(dados$Q006) ))
            temp$Q006 <- factor(temp$Q006, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"))
            temp <- as.data.frame(table(temp$Q006))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q006", values=c ("#005fff","#007fff","#009fff","#00afff","#00cfff","#00efff","#00ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff","#01ffff")) +
                #scale_fill_manual("Q006", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q006", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")



        } else{ if(input$variableQ=="Q007") {
            temp <- dados %>%
                mutate(Q007 = ifelse(Q007=="", NA, Q007)) %>%
                mutate(Q007 = factor(Q007, labels = unique(dados$Q007) ))
            temp$Q007 <- factor(temp$Q007, labels = c("A", "B", "C", "D"))
            temp <- as.data.frame(table(temp$Q007))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q007", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q007", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")


        } else{ if(input$variableQ=="Q008") {
            temp <- dados %>%
                mutate(Q008 = ifelse(Q008=="", NA, Q008)) %>%
                mutate(Q008 = factor(Q008, labels = unique(dados$Q008) ))
            temp$Q008 <- factor(temp$Q008, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q008))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q008", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q008", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q009") {
            temp <- dados %>%
                mutate(Q009 = ifelse(Q009=="", NA, Q009)) %>%
                mutate(Q009 = factor(Q009, labels = unique(dados$Q009) ))
            temp$Q009 <- factor(temp$Q009, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q009))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q009", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q009", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q010") {
            temp <- dados %>%
                mutate(Q010 = ifelse(Q010=="", NA, Q010)) %>%
                mutate(Q010 = factor(Q010, labels = unique(dados$Q010) ))
            temp$Q010 <- factor(temp$Q010, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q010))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q010", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q010", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q011") {
            temp <- dados %>%
                mutate(Q011 = ifelse(Q011=="", NA, Q011)) %>%
                mutate(Q011 = factor(Q011, labels = unique(dados$Q011) ))
            temp$Q011 <- factor(temp$Q011, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q011))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q011", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q011", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q012") {
            temp <- dados %>%
                mutate(Q012 = ifelse(Q012=="", NA, Q012)) %>%
                mutate(Q012 = factor(Q012, labels = unique(dados$Q012) ))
            temp$Q010 <- factor(temp$Q012, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q012))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q012", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q012", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q013") {
            temp <- dados %>%
                mutate(Q013 = ifelse(Q013=="", NA, Q013)) %>%
                mutate(Q013 = factor(Q013, labels = unique(dados$Q013) ))
            temp$Q013 <- factor(temp$Q013, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q013))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q013", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q013", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q014") {
            temp <- dados %>%
                mutate(Q014 = ifelse(Q014=="", NA, Q014)) %>%
                mutate(Q014 = factor(Q014, labels = unique(dados$Q014) ))
            temp$Q014 <- factor(temp$Q014, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q014))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q014", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q014", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")


        } else{ if(input$variableQ=="Q015") {
            temp <- dados %>%
                mutate(Q015 = ifelse(Q015=="", NA, Q015)) %>%
                mutate(Q015 = factor(Q015, labels = unique(dados$Q015) ))
            temp$Q015 <- factor(temp$Q015, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q015))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q015", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q015", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q016") {
            temp <- dados %>%
                mutate(Q016 = ifelse(Q016=="", NA, Q016)) %>%
                mutate(Q016 = factor(Q016, labels = unique(dados$Q016) ))
            temp$Q016 <- factor(temp$Q016, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q016))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q016", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q016", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q017") {
            temp <- dados %>%
                mutate(Q017 = ifelse(Q017=="", NA, Q017)) %>%
                mutate(Q017 = factor(Q017, labels = unique(dados$Q017) ))
            temp$Q017 <- factor(temp$Q017, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q017))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q017", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q017", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q018") {
            temp <- dados %>%
                mutate(Q018 = ifelse(Q018=="", NA, Q018)) %>%
                mutate(Q018 = factor(Q018, labels = unique(dados$Q018) ))
            temp$Q018 <- factor(temp$Q018, labels = c("A", "B"))
            temp <- as.data.frame(table(temp$Q018))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q018", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q018", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q019") {
            temp <- dados %>%
                mutate(Q019 = ifelse(Q019=="", NA, Q019)) %>%
                mutate(Q019 = factor(Q019, labels = unique(dados$Q019) ))
            temp$Q019 <- factor(temp$Q019, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q019))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q019", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q019", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q020") {
            temp <- dados %>%
                mutate(Q020 = ifelse(Q020=="", NA, Q020)) %>%
                mutate(Q020 = factor(Q020, labels = unique(dados$Q020) ))
            temp$Q020 <- factor(temp$Q020, labels = c("A", "B"))
            temp <- as.data.frame(table(temp$Q020))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q020", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q020", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q021") {
            temp <- dados %>%
                mutate(Q021 = ifelse(Q021=="", NA, Q021)) %>%
                mutate(Q021 = factor(Q021, labels = unique(dados$Q021) ))
            temp$Q021 <- factor(temp$Q021, labels = c("A", "B"))
            temp <- as.data.frame(table(temp$Q021))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q021", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q021", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q022") {
            temp <- dados %>%
                mutate(Q022 = ifelse(Q022=="", NA, Q022)) %>%
                mutate(Q022 = factor(Q022, labels = unique(dados$Q022) ))
            temp$Q022 <- factor(temp$Q022, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q022))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q022", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q022", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q023") {
            temp <- dados %>%
                mutate(Q023 = ifelse(Q023=="", NA, Q023)) %>%
                mutate(Q023 = factor(Q023, labels = unique(dados$Q023) ))
            temp$Q023 <- factor(temp$Q023, labels = c("A", "B"))
            temp <- as.data.frame(table(temp$Q023))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q023", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q023", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q024") {
            temp <- dados %>%
                mutate(Q024 = ifelse(Q024=="", NA, Q024)) %>%
                mutate(Q024 = factor(Q024, labels = unique(dados$Q024) ))
            temp$Q024 <- factor(temp$Q024, labels = c("A", "B", "C", "D", "E"))
            temp <- as.data.frame(table(temp$Q024))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q024", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q024", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ if(input$variableQ=="Q025") {
          temp <- dados %>%
            mutate(Q025 = ifelse(Q025=="", NA, Q025)) %>%
            mutate(Q025 = factor(Q025, labels = unique(dados$Q025) ))
          temp$Q025 <- factor(temp$Q025, labels = c("A", "B"))
          temp <- as.data.frame(table(temp$Q025))
          tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                        text=paste("Resposta: ", Var1, "<br>",
                                                   "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
            geom_bar(stat = "identity") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
            scale_fill_manual("Q025", values=brewer.pal(9, "Paired")) +
            ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
            ggplot2::labs(x="Q025", y="Número de Candidatos")+
            theme_minimal() +
            theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
          plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
            style(textposition = "top")

        } else{ if(input$variableQ=="Q026") {
          temp <- dados %>%
            mutate(Q026 = ifelse(Q026=="", NA, Q026)) %>%
            mutate(Q026 = factor(Q026, labels = unique(dados$Q007) ))
          temp$Q026 <- factor(temp$Q026, labels = c("A", "B", "C", "D"))
          temp <- as.data.frame(table(temp$Q026))
          tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                        text=paste("Resposta: ", Var1, "<br>",
                                                   "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
            geom_bar(stat = "identity") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
            scale_fill_manual("Q026", values=brewer.pal(9, "Paired")) +
            ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
            ggplot2::labs(x="Q026", y="Número de Candidatos")+
            theme_minimal() +
            theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
          plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
            style(textposition = "top")

#        } else{ if(input$variableQ=="Q026") {
#            temp <- dados %>%
#                mutate(Q026 = ifelse(Q026=="", NA, Q026)) %>%
#                mutate(Q026 = factor(Q026, labels = unique(dados$Q026) ))
#            temp$Q026 <- factor(temp$Q026, levels = c("A", "B", "C", "D"))
#            temp <- as.data.frame(table(temp$Q026))
#            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
#                                          text=paste("Resposta: ", Var1, "<br>",
#                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
#                geom_bar(stat = "identity") +
#                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
#                scale_fill_manual("Q026", values=brewer.pal(9, "Paired")) +
#                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
#                ggplot2::labs(x="Q026", y="Número de Candidatos")+
#                theme_minimal() +
#                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
#            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
#                style(textposition = "top")

        } else{ if(input$variableQ=="Q027") {
            temp <- dados %>%
                mutate(Q027 = ifelse(Q027=="", NA, Q027)) %>%
                mutate(Q027 = factor(Q027, labels = unique(dados$Q027) ))
            temp$Q027 <- factor(temp$Q027, labels = c("A", "B", "C", "D", "E", "F"))
            temp <- as.data.frame(table(temp$Q027))
            tplot3 <- temp %>% ggplot2::ggplot(aes(x = Var1, y=Freq, fill=Var1,
                                          text=paste("Resposta: ", Var1, "<br>",
                                                     "Quant: ", Freq), label = sprintf("%.02f %%", Freq/sum(Freq)*100))) +
                geom_bar(stat = "identity") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
                scale_fill_manual("Q027", values=brewer.pal(9, "Paired")) +
                ggplot2::geom_text(position = position_dodge(width = 0.9), vjust=-0.3)+
                ggplot2::labs(x="Q027", y="Número de Candidatos")+
                theme_minimal() +
                theme( axis.text.x  = element_text(angle=0, vjust=0.5, size=10), legend.position="none")
            plotly::ggplotly(tplot3, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>%
                style(textposition = "top")

        } else{ ##

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

    })

    output$tabQ001 <- renderTable({

        if(input$variableQ=="Q001") {

            tabQ001 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F", "G", "H"),
                                  "Significado"=c("Nunca estudou",
                                                  "Não completou a 4ª série/5º ano do Ensino Fundamental",
                                                  "Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental",
                                                  "Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio",
                                                  "Completou o Ensino Médio, mas não completou a Faculdade",
                                                  "Completou a Faculdade, mas não completou a Pós-graduação",
                                                  "Completou a Pós-graduação",
                                                  "Não sei"))
            tabQ001

        } else {
            if(input$variableQ=="Q002") {

                tabQ002 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F", "G", "H"),
                                      "Significado"=c("Nunca estudou",
                                                      "Não completou a 4ª série/5º ano do Ensino Fundamental",
                                                      "Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental",
                                                      "Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio",
                                                      "Completou o Ensino Médio, mas não completou a Faculdade",
                                                      "Completou a Faculdade, mas não completou a Pós-graduação",
                                                      "Completou a Pós-graduação",
                                                      "Não sei"))
                tabQ002

            } else{ if(input$variableQ=="Q003") {

                tabQ003 <- data.frame("Resposta"=c("Grupo A", "Grupo B", "Grupo C", "Grupo D", "Grupo E", "Grupo F"),
                                      "Significado"=c("Lavrador, agricultor sem empregados, boia-fria, criador de animais (gado, porcos, galinhas, ovelhas, cavalos etc.), apicultor, pescador, lenhador, seringueiro, extrativista.",
                                                      "Diarista, empregado doméstico, cuidador de idosos, babá, cozinheiro (em casas particulares), motorista particular, jardineiro, faxineiro de empresas e prédios, vigilante, porteiro, carteiro, office-boy, vendedor, caixa, atendente de loja, auxiliar administrativo, recepcionista, servente de pedreiro, repositor de mercadoria",
                                                      "Padeiro, cozinheiro industrial ou em restaurantes, sapateiro, costureiro, joalheiro, torneiro mecânico, operador de máquinas, soldador, operário de fábrica, trabalhador da mineração, pedreiro, pintor, eletricista, encanador, motorista, caminhoneiro, taxista.",
                                                      "Professor (de ensino fundamental ou médio, idioma, música, artes etc.), técnico (de enfermagem, contabilidade, eletrônica etc.), policial, militar de baixa patente (soldado, cabo, sargento), corretor de imóveis, supervisor, gerente, mestre de obras, pastor, microempresário (proprietário de empresa com menos de 10 empregados), pequeno comerciante, pequeno proprietário de terras, trabalhador autônomo ou por conta própria.",
                                                      "Médico, engenheiro, dentista, psicólogo, economista, advogado, juiz, promotor, defensor, delegado, tenente, capitão, coronel, professor universitário, diretor em empresas públicas ou privadas, político, proprietário de empresas com mais de 10 empregados.",
                                                      "Não sei"))
                tabQ003

                } else{ if(input$variableQ=="Q004") {
                    tabQ004 <- data.frame("Resposta"=c("Grupo A", "Grupo B", "Grupo C", "Grupo D", "Grupo E", "Grupo F"),
                                          "Significado"=c("Lavradora, agricultora sem empregados, boia-fria, criadora de animais (gado, porcos, galinhas, ovelhas, cavalos etc.), apicultora, pescadora, lenhadora, seringueira, extrativista.",
                                                          "Diarista, empregada doméstica, cuidadora de idosos, babá, cozinheira (em casas particulares), motorista particular, jardineira, faxineira de empresas e prédios, vigilante, porteira, carteira, office-girl, vendedora, caixa, atendente de loja, auxiliar administrativo, recepcionista, servente de pedreiro, repositora de mercadoria",
                                                          "Padeira, cozinheira industrial ou em restaurantes, sapateira, costureira, joalheira, torneira mecânica, operadora de máquinas, soldadora, operária de fábrica, trabalhadora da mineração, pedreira, pintora, eletricista, encanadora, motorista, caminhoneira, taxista.",
                                                          "Professora (de ensino fundamental ou médio, idioma, música, artes etc.), técnica (de enfermagem, contabilidade, eletrônica etc.), policial, militar de baixa patente (soldado, cabo, sargento), corretora de imóveis, supervisora, gerente, mestre de obras, pastora, microempresária (proprietário de empresa com menos de 10 empregados), pequena comerciante, pequena proprietária de terras, trabalhadora autônoma ou por conta própria.",
                                                          "Médica, engenheira, dentista, psicóloga, economista, advogada, juiza, promotora, defensora, delegada, tenente, capitã, coronel, professora universitária, diretora em empresas públicas ou privadas, política, proprietária de empresas com mais de 10 empregados.",
                                                          "Não sei"))
                    tabQ004

                } else{ if(input$variableQ=="Q005") {
                    tabQ005 <- data.frame("Resposta"=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"),
                                          "Significado"=c("1 pessoa, pois moro sozinho(a)", "2 pessoas", "3 pessoas", "4 pessoas", "5 pessoas", "6 pessoas", "7 pessoas", "8 pessoas", "9 pessoas", "10 pessoas", "11 pessoas", "12 pessoas", "13 pessoas", "14 pessoas", "15 pessoas", "16 pessoas", "17 pessoas", "18 pessoas", "19 pessoas", "20 pessoas"))
                    tabQ005


                } else{ if(input$variableQ=="Q006") {
                    tabQ006 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"),
                                          "Significado"=c("Nenhuma renda.","Até R$ 954,00.","De R$ 954,01 até R$ 1.431,00.","De R$ 1.431,01 até R$ 1.908,00.","De R$ 1.908,01 até R$ 2.385,00.","De R$ 2.385,01 até R$ 2.862,00.","De R$ 2.862,01 até R$ 3.816,00.","De R$ 3.816,01 até R$ 4.770,00.",
                                                          "De R$ 4.770,01 até R$ 5.724,00.","De R$ 5.724,01 até R$ 6.678,00.","De R$ 6.678,01 até R$ 7.632,00.","De R$ 7.632,01 até R$ 8.586,00.","De R$ 8.586,01 até R$ 9.540,00.","De R$ 9.540,01 até R$ 11.448,00.","De R$ 11.448,01 até R$ 14.310,00.",
                                                          "De R$ 14.310,01 até R$ 19.080,00.","Mais de R$ 19.080,00."))
                    tabQ006


                } else{ if(input$variableQ=="Q007") {
                    tabQ007 <- data.frame("Resposta"=c("A", "B", "C", "D"),
                                          "Significado"=c("Não","Sim, um ou dois dias por semana.","Sim, três ou quatro dias por semana.","Sim, pelo menos cinco dias por semana."))
                    tabQ007

                } else{ if(input$variableQ=="Q008") {
                    tabQ008 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ008


                } else{ if(input$variableQ=="Q009") {
                    tabQ009 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ009


                } else{ if(input$variableQ=="Q010") {
                    tabQ010 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ010


                } else{ if(input$variableQ=="Q011") {
                    tabQ011 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ011


                } else{ if(input$variableQ=="Q012") {
                    tabQ012 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ012


                } else{ if(input$variableQ=="Q013") {
                    tabQ013 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ013


                } else{ if(input$variableQ=="Q014") {
                    tabQ014 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ014


                } else{ if(input$variableQ=="Q015") {
                    tabQ015 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ015


                } else{ if(input$variableQ=="Q016") {
                    tabQ016 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ016


                } else{ if(input$variableQ=="Q017") {
                    tabQ017 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ017


                } else{ if(input$variableQ=="Q018") {
                    tabQ018 <- data.frame("Resposta"=c("A", "B"),
                                          "Significado"=c("Não","Sim"))
                    tabQ018


                } else{ if(input$variableQ=="Q019") {
                    tabQ019 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ019


                } else{ if(input$variableQ=="Q020") {
                    tabQ020 <- data.frame("Resposta"=c("A", "B"),
                                          "Significado"=c("Não","Sim"))
                    tabQ020

                } else{ if(input$variableQ=="Q021") {
                    tabQ021 <- data.frame("Resposta"=c("A", "B"),
                                          "Significado"=c("Não","Sim"))
                    tabQ021

                } else{ if(input$variableQ=="Q022") {
                    tabQ022 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ022


                } else{ if(input$variableQ=="Q023") {
                    tabQ023 <- data.frame("Resposta"=c("A", "B"),
                                          "Significado"=c("Não","Sim"))
                    tabQ023

                } else{ if(input$variableQ=="Q024") {
                    tabQ024 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                          "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
                    tabQ024

                } else{ if(input$variableQ=="Q025") {
                    tabQ025 <- data.frame("Resposta"=c("A", "B"),
                                          "Significado"=c("Não","Sim"))
                    tabQ025

                } else{ if(input$variableQ=="Q026") {
                    tabQ026 <- data.frame("Resposta"=c("A", "B", "C", "D"),
                                          "Significado"=c("Já concluí o Ensino Médio.","Estou cursando e concluirei o Ensino Médio em 2018.",
                                                          "Estou cursando e concluirei o Ensino Médio após 2018.","Não concluí e não estou cursando o Ensino Médio."))
                    tabQ026

                } else{ if(input$variableQ=="Q027") {
                    tabQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                          "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                          "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                          "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
                    tabQ027

                } else{ ##

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }

                }
            }




    })



    ################ QUESTIONÁRIO SOCIOECONÔMICO2 #####################
    ##################################################################

    output$TextQ00222 <- renderText({
      if(input$variableQ2=="Q001") {
        "Até que série seu pai, ou o homem responsável por você, estudou?"
      } else {
        if(input$variableQ2=="Q002") {
          "Até que série sua mãe, ou a mulher responsável por você, estudou?"
        } else {
          if(input$variableQ2=="Q003") {
            "indique o grupo que contempla a ocupação mais próxima da ocupação do seu pai ou do homem responsável por você. (Se ele não estiver trabalhando, escolha uma ocupação pensando no último trabalho dele)"
          } else {
            if(input$variableQ2=="Q004") {
              "indique o grupo que contempla a ocupação mais próxima da ocupação da sua mãe ou da mulher responsável por você. (Se ela não estiver trabalhando, escolha uma ocupação pensando no último trabalho dela)"
            } else {
              if(input$variableQ2=="Q005") {
                "Incluindo você, quantas pessoas moram atualmente em sua residência?"
              } else {
                if(input$variableQ2=="Q006") {
                  "Qual é a renda mensal de sua família, somando a sua renda com a dos seus familiares?"
                } else {
                  if(input$variableQ2=="Q007") {
                    "Em sua residência trabalha empregado(a) doméstico(a)?"
                  } else {
                    if(input$variableQ2=="Q008") {
                      "Na sua residência tem banheiro?"
                    } else {
                      if(input$variableQ2=="Q009") {
                        "Na sua residência tem quartos para dormir?"
                      } else {
                        if(input$variableQ2=="Q010") {
                          "Na sua residência tem carro?"
                        } else {
                          if(input$variableQ2=="Q011") {
                            "Na sua residência tem motocicleta?"
                          } else {
                            if(input$variableQ2=="Q012") {
                              "Na sua residência tem geladeira?"
                            } else {
                              if(input$variableQ2=="Q013") {
                                "Na sua residência tem freezer (independente ou segunda porta da geladeira)?"
                              } else {
                                if(input$variableQ2=="Q014") {
                                  "Na sua residência tem máquina de lavar roupa?"
                                } else {
                                  if(input$variableQ2=="Q015") {
                                    "Na sua residência tem máquina de secar roupa (independente ou em conjunto com a máquina de lavar roupa)?"
                                  } else {
                                    if(input$variableQ2=="Q016") {
                                      "Na sua residência tem forno micro-ondas?"
                                    } else {
                                      if(input$variableQ2=="Q017") {
                                        "Na sua residência tem máquina de lavar louça?"
                                      } else {
                                        if(input$variableQ2=="Q018") {
                                          "Na sua residência tem aspirador de pó?"
                                        } else {
                                          if(input$variableQ2=="Q019") {
                                            "Na sua residência tem televisão em cores?"
                                          } else {
                                            if(input$variableQ2=="Q020") {
                                              "Na sua residência tem aparelho de DVD?"
                                            } else {
                                              if(input$variableQ2=="Q021") {
                                                "Na sua residência tem TV por assinatura?"
                                              } else {
                                                if(input$variableQ2=="Q022") {
                                                  "Na sua residência tem telefone celular?"
                                                } else {
                                                  if(input$variableQ2=="Q023") {
                                                    "Na sua residência tem telefone fixo?"
                                                  } else {
                                                    if(input$variableQ2=="Q024") {
                                                      "Na sua residência tem computador?"
                                                    } else {
                                                      if(input$variableQ2=="Q025") {
                                                        "Na sua residência tem acesso à Internet?"
                                                      } else {
                                                        if(input$variableQ2=="Q026") {
                                                          "Você já concluiu ou está concluindo o Ensino Médio?"
                                                        } else {
                                                          if(input$variableQ2=="Q027") {
                                                            "Em que tipo de escola você frequentou o Ensino Médio?"
                                                          } else {
                                                            ##
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    })


    output$Q00222 <- renderPlotly({



            if(input$variableQ2=="Q001") {
              dados3 <- dados %>% dplyr::filter(!is.na(Q001)&!is.na(nt_final)) %>%
                dplyr::group_by(Q001) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
              names(dados3)<-c("UF","Media","Quant")
              #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
              p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                          fill=UF, label=Media,
                                          text=paste("Estado: ", UF, "<br>",
                                                     "Nota Média: ", Media, "<br>",
                                                     "Quantidade: ", Quant))) +
                ggplot2::geom_col(show.legend = FALSE)+
                ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                              label = paste0(round(Media, digits = 2)," ")), size=4)+
                ggplot2::labs(x="Q001",
                     y="Nota Média",
                     title = "")
              plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


      } else{ if(input$variableQ2=="Q002") {

        dados3 <- dados %>% dplyr::filter(!is.na(Q002)&!is.na(nt_final)) %>%
          dplyr::group_by(Q002) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q002",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q003") {

        dados3 <- dados %>% dplyr::filter(!is.na(Q003)&!is.na(nt_final)) %>%
          dplyr::group_by(Q003) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q003",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q004") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q004)&!is.na(nt_final)) %>%
          dplyr::group_by(Q004) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q004",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


      } else{ if(input$variableQ2=="Q005") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q005)&!is.na(nt_final)) %>%
          dplyr::group_by(Q005) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c(1-20), Media=dados3$Media, Quant=dados3$Quant)
        dados3 <- data.frame(UF=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q005",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


      } else{ if(input$variableQ2=="Q006") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q006)&!is.na(nt_final)) %>%
          dplyr::group_by(Q006) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q006",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")



      } else{ if(input$variableQ2=="Q007") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q007)&!is.na(nt_final)) %>%
          dplyr::group_by(Q007) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q007",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


      } else{ if(input$variableQ2=="Q008") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q008)&!is.na(nt_final)) %>%
          dplyr::group_by(Q008) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q008",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q009") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q009)&!is.na(nt_final)) %>%
          dplyr::group_by(Q009) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q009",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q010") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q010)&!is.na(nt_final)) %>%
          dplyr::group_by(Q010) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q010",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q011") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q011)&!is.na(nt_final)) %>%
          dplyr::group_by(Q011) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q011",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q012") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q012)&!is.na(nt_final)) %>%
          dplyr::group_by(Q012) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q012",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q013") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q013)&!is.na(nt_final)) %>%
          dplyr::group_by(Q013) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q013",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q014") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q014)&!is.na(nt_final)) %>%
          dplyr::group_by(Q014) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q014",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")


      } else{ if(input$variableQ2=="Q015") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q015)&!is.na(nt_final)) %>%
          dplyr::group_by(Q015) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q015",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q016") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q016)&!is.na(nt_final)) %>%
          dplyr::group_by(Q016) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q016",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q017") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q017)&!is.na(nt_final)) %>%
          dplyr::group_by(Q017) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q017",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q018") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q018)&!is.na(nt_final)) %>%
          dplyr::group_by(Q018) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q018",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q019") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q019)&!is.na(nt_final)) %>%
          dplyr::group_by(Q019) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q019",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q020") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q020)&!is.na(nt_final)) %>%
          dplyr::group_by(Q020) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q020",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q021") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q021)&!is.na(nt_final)) %>%
          dplyr::group_by(Q021) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q021",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q022") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q022)&!is.na(nt_final)) %>%
          dplyr::group_by(Q022) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q022",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q023") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q023)&!is.na(nt_final)) %>%
          dplyr::group_by(Q023) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q023",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q024") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q024)&!is.na(nt_final)) %>%
          dplyr::group_by(Q024) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q024",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q025") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q025)&!is.na(nt_final)) %>%
          dplyr::group_by(Q025) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q025",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q026") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q026)&!is.na(nt_final)) %>%
          dplyr::group_by(Q026) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q026",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ if(input$variableQ2=="Q027") {
        dados3 <- dados %>% dplyr::filter(!is.na(Q027)&!is.na(nt_final)) %>%
          dplyr::group_by(Q027) %>% dplyr::summarise(Media=mean(nt_final),Quant=n())
        names(dados3)<-c("UF","Media","Quant")
        #dados3 <- data.frame(UF=c("A", "B", "C", "D", "E", "F", "G", "H" ), Media=dados3$Media, Quant=dados3$Quant)
        p1 <- dados3 %>% ggplot2::ggplot(aes(reorder(UF, UF), Media,
                                    fill=UF, label=Media,
                                    text=paste("Estado: ", UF, "<br>",
                                               "Nota Média: ", Media, "<br>",
                                               "Quantidade: ", Quant))) +
          ggplot2::geom_col(show.legend = FALSE)+
          ggplot2::geom_text(aes(x = UF, y = round(Media, digits = 2),
                        label = paste0(round(Media, digits = 2)," ")), size=4)+
          ggplot2::labs(x="Q027",
               y="Nota Média",
               title = "")
        plotly::ggplotly(p1, tooltip = "text") %>% plotly::layout(showlegend = FALSE)%>% style(textposition = "top")

      } else{ ##

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

      }

    })

    output$tabQ001222 <- renderTable({

      if(input$variableQ2=="Q001") {

        tabQ001 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F", "G", "H"),
                              "Significado"=c("Nunca estudou",
                                              "Não completou a 4ª série/5º ano do Ensino Fundamental",
                                              "Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental",
                                              "Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio",
                                              "Completou o Ensino Médio, mas não completou a Faculdade",
                                              "Completou a Faculdade, mas não completou a Pós-graduação",
                                              "Completou a Pós-graduação",
                                              "Não sei"))
        tabQ001

      } else {
        if(input$variableQ2=="Q002") {

          tabQ002 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F", "G", "H"),
                                "Significado"=c("Nunca estudou",
                                                "Não completou a 4ª série/5º ano do Ensino Fundamental",
                                                "Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental",
                                                "Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio",
                                                "Completou o Ensino Médio, mas não completou a Faculdade",
                                                "Completou a Faculdade, mas não completou a Pós-graduação",
                                                "Completou a Pós-graduação",
                                                "Não sei"))
          tabQ002

        } else{ if(input$variableQ2=="Q003") {

          tabQ003 <- data.frame("Resposta"=c("Grupo A", "Grupo B", "Grupo C", "Grupo D", "Grupo E", "Grupo F"),
                                "Significado"=c("Lavrador, agricultor sem empregados, boia-fria, criador de animais (gado, porcos, galinhas, ovelhas, cavalos etc.), apicultor, pescador, lenhador, seringueiro, extrativista.",
                                                "Diarista, empregado doméstico, cuidador de idosos, babá, cozinheiro (em casas particulares), motorista particular, jardineiro, faxineiro de empresas e prédios, vigilante, porteiro, carteiro, office-boy, vendedor, caixa, atendente de loja, auxiliar administrativo, recepcionista, servente de pedreiro, repositor de mercadoria",
                                                "Padeiro, cozinheiro industrial ou em restaurantes, sapateiro, costureiro, joalheiro, torneiro mecânico, operador de máquinas, soldador, operário de fábrica, trabalhador da mineração, pedreiro, pintor, eletricista, encanador, motorista, caminhoneiro, taxista.",
                                                "Professor (de ensino fundamental ou médio, idioma, música, artes etc.), técnico (de enfermagem, contabilidade, eletrônica etc.), policial, militar de baixa patente (soldado, cabo, sargento), corretor de imóveis, supervisor, gerente, mestre de obras, pastor, microempresário (proprietário de empresa com menos de 10 empregados), pequeno comerciante, pequeno proprietário de terras, trabalhador autônomo ou por conta própria.",
                                                "Médico, engenheiro, dentista, psicólogo, economista, advogado, juiz, promotor, defensor, delegado, tenente, capitão, coronel, professor universitário, diretor em empresas públicas ou privadas, político, proprietário de empresas com mais de 10 empregados.",
                                                "Não sei"))
          tabQ003

        } else{ if(input$variableQ2=="Q004") {
          tabQ004 <- data.frame("Resposta"=c("Grupo A", "Grupo B", "Grupo C", "Grupo D", "Grupo E", "Grupo F"),
                                "Significado"=c("Lavradora, agricultora sem empregados, boia-fria, criadora de animais (gado, porcos, galinhas, ovelhas, cavalos etc.), apicultora, pescadora, lenhadora, seringueira, extrativista.",
                                                "Diarista, empregada doméstica, cuidadora de idosos, babá, cozinheira (em casas particulares), motorista particular, jardineira, faxineira de empresas e prédios, vigilante, porteira, carteira, office-girl, vendedora, caixa, atendente de loja, auxiliar administrativo, recepcionista, servente de pedreiro, repositora de mercadoria",
                                                "Padeira, cozinheira industrial ou em restaurantes, sapateira, costureira, joalheira, torneira mecânica, operadora de máquinas, soldadora, operária de fábrica, trabalhadora da mineração, pedreira, pintora, eletricista, encanadora, motorista, caminhoneira, taxista.",
                                                "Professora (de ensino fundamental ou médio, idioma, música, artes etc.), técnica (de enfermagem, contabilidade, eletrônica etc.), policial, militar de baixa patente (soldado, cabo, sargento), corretora de imóveis, supervisora, gerente, mestre de obras, pastora, microempresária (proprietário de empresa com menos de 10 empregados), pequena comerciante, pequena proprietária de terras, trabalhadora autônoma ou por conta própria.",
                                                "Médica, engenheira, dentista, psicóloga, economista, advogada, juiza, promotora, defensora, delegada, tenente, capitã, coronel, professora universitária, diretora em empresas públicas ou privadas, política, proprietária de empresas com mais de 10 empregados.",
                                                "Não sei"))
          tabQ004

        } else{ if(input$variableQ2=="Q005") {
          tabQ005 <- data.frame("Resposta"=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"),
                                "Significado"=c("1 pessoa, pois moro sozinho(a)", "2 pessoas", "3 pessoas", "4 pessoas", "5 pessoas", "6 pessoas", "7 pessoas", "8 pessoas", "9 pessoas", "10 pessoas", "11 pessoas", "12 pessoas", "13 pessoas", "14 pessoas", "15 pessoas", "16 pessoas", "17 pessoas", "18 pessoas", "19 pessoas", "20 pessoas"))
          tabQ005


        } else{ if(input$variableQ2=="Q006") {
          tabQ006 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"),
                                "Significado"=c("Nenhuma renda.","Até R$ 954,00.","De R$ 954,01 até R$ 1.431,00.","De R$ 1.431,01 até R$ 1.908,00.","De R$ 1.908,01 até R$ 2.385,00.","De R$ 2.385,01 até R$ 2.862,00.","De R$ 2.862,01 até R$ 3.816,00.","De R$ 3.816,01 até R$ 4.770,00.",
                                                "De R$ 4.770,01 até R$ 5.724,00.","De R$ 5.724,01 até R$ 6.678,00.","De R$ 6.678,01 até R$ 7.632,00.","De R$ 7.632,01 até R$ 8.586,00.","De R$ 8.586,01 até R$ 9.540,00.","De R$ 9.540,01 até R$ 11.448,00.","De R$ 11.448,01 até R$ 14.310,00.",
                                                "De R$ 14.310,01 até R$ 19.080,00.","Mais de R$ 19.080,00."))
          tabQ006


        } else{ if(input$variableQ2=="Q007") {
          tabQ007 <- data.frame("Resposta"=c("A", "B", "C", "D"),
                                "Significado"=c("Não","Sim, um ou dois dias por semana.","Sim, três ou quatro dias por semana.","Sim, pelo menos cinco dias por semana."))
          tabQ007

        } else{ if(input$variableQ2=="Q008") {
          tabQ008 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
          tabQ008


        } else{ if(input$variableQ2=="Q009") {
          tabQ009 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
          tabQ009


        } else{ if(input$variableQ2=="Q010") {
          tabQ010 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
          tabQ010


        } else{ if(input$variableQ2=="Q011") {
          tabQ011 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
          tabQ011


        } else{ if(input$variableQ2=="Q012") {
          tabQ012 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
          tabQ012


        } else{ if(input$variableQ2=="Q013") {
          tabQ013 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
          tabQ013


        } else{ if(input$variableQ2=="Q014") {
          tabQ014 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
          tabQ014


        } else{ if(input$variableQ2=="Q015") {
          tabQ015 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
          tabQ015


        } else{ if(input$variableQ2=="Q016") {
          tabQ016 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
          tabQ016


        } else{ if(input$variableQ2=="Q017") {
          tabQ017 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
          tabQ017


        } else{ if(input$variableQ2=="Q018") {
          tabQ018 <- data.frame("Resposta"=c("A", "B"),
                                "Significado"=c("Não","Sim"))
          tabQ018


        } else{ if(input$variableQ2=="Q019") {
          tabQ019 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, uma.","Sim, duas.","Sim, três.", "Sim, quatro ou mais"))
          tabQ019


        } else{ if(input$variableQ2=="Q020") {
          tabQ020 <- data.frame("Resposta"=c("A", "B"),
                                "Significado"=c("Não","Sim"))
          tabQ020

        } else{ if(input$variableQ2=="Q021") {
          tabQ021 <- data.frame("Resposta"=c("A", "B"),
                                "Significado"=c("Não","Sim"))
          tabQ021

        } else{ if(input$variableQ2=="Q022") {
          tabQ022 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
          tabQ022


        } else{ if(input$variableQ2=="Q023") {
          tabQ023 <- data.frame("Resposta"=c("A", "B"),
                                "Significado"=c("Não","Sim"))
          tabQ023

        } else{ if(input$variableQ2=="Q024") {
          tabQ024 <- data.frame("Resposta"=c("A", "B", "C", "D", "E"),
                                "Significado"=c("Não","Sim, um.","Sim, dois.","Sim, três.", "Sim, quatro ou mais"))
          tabQ024

        } else{ if(input$variableQ2=="Q025") {
          tabQ025 <- data.frame("Resposta"=c("A", "B"),
                                "Significado"=c("Não","Sim"))
          tabQ025

        } else{ if(input$variableQ2=="Q026") {
          tabQ026 <- data.frame("Resposta"=c("A", "B", "C", "D"),
                                "Significado"=c("Já concluí o Ensino Médio.","Estou cursando e concluirei o Ensino Médio em 2018.",
                                                "Estou cursando e concluirei o Ensino Médio após 2018.","Não concluí e não estou cursando o Ensino Médio."))
          tabQ026

        } else{ if(input$variableQ2=="Q027") {
          tabQ027 <- data.frame("Resposta"=c("A", "B", "C", "D", "E", "F"),
                                "Significado"=c("Somente em escola pública.","Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.",
                                                "Parte em escola pública e parte em escola privada COM bolsa de estudo integral.","Somente em escola privada SEM bolsa de estudo integral.",
                                                "Somente em escola privada COM bolsa de estudo integral.","Não frequentei a escola"))
          tabQ027

        } else{ ##

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }

        }
      }




    })









})


















































