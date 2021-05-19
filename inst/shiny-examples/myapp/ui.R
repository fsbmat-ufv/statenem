#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    navbarPage(title="Enem 2018",
               tabPanel("Descrição",
                        tabsetPanel(
                          tabPanel("Sobre o Dashboard",

                                   fluidRow(
                                     column(12,
                                            h1("Sobre o Dashboard"),
                                            sidebarPanel(
                                              p(style="text-align: justify;","Este dashboard é resultado do trabalho de
                                        dissertação de mestrado da estudante do Curso de Mestrado Profissional
                                        em Rede Nacional Eveline Júnia Brant Mariz. Orientada pelo professor
                                        Fernando de Souza Bastos, ambos da Universidade Federal de Viçosa - campus UFV - Florestal."),
                                              p(style="text-align: justify;","O código utilizado para construir
                                          a aplicação está disponível em um repositório aberto no GitHub
                                          (https://github.com/fsbmat-ufv/statenem) e pode futuramente ser
                                          replicado facilmente para novas atualizações do Exame Nacional do Ensino Médio (ENEM)."),
                                              br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                            ),
                                            h4(style="text-align: justify;","   O dashboard tem o objetivo de apresentar
                                            uma Estatística Descritiva Básica dos dados fornecidos pelo Instituto
                                            Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira (INEP),
                                            relativo ao ENEM de 2018. O banco de dados inclui todos os candidatos que
                                            compareceram em todas as etapas do exame daquele ano, totalizando 3.893.743
                                            (três milhões, oitocentos e noventa e três Mil, setecentos e quarenta e três)
                                            candidatos. No presente estudo, o cálculo da nota final considerou peso 1
                                            para todas as áreas do conhecimento, independente do curso almejado pelo
                                            candidato, conforme critério utilizado pela Universidade Federal de Viçosa
                                            (UFV) para ingresso no ano subsequente a aplicação do exame.

                                            Assim, a nota final de cada candidato foi calculada conforme a média ponderada
                                            (ou aritmética, no caso) das notas obtidas na redação e nas quatro áreas do
                                            conhecimento, a saber, Ciências da Natureza, Ciências Humanas, Linguagens e
                                            Códigos e Matemática."),
                                     ))))# barra de navegacao interna
               ),
               #
               tabPanel("Dados do Participante",
                        tabsetPanel(
                            tabPanel("Sample Data",
                                     fluidRow(
                                         column(12,
                                                DT::dataTableOutput("table1")))),
                            ##
                            tabPanel("Idade",
                                     fluidRow(column(9,
                                                     plotlyOutput("idade", height = 600),
                                                     tableOutput("tabidade")))),
                            ##
                            tabPanel("Gênero",
                                     fluidRow(column(3,
                                                     selectInput("variableGE",
                                                                 strong("Escolha uma opção:"),
                                                                 choices=c("Gênero"="GE",
                                                                           "Média de Idade por Gênero"="GE2"),
                                                                 selected = "GE")),
                                              column(9,
                                                     plotlyOutput("GE", height = 600)))),
                            ##
                            tabPanel("Estado Civil",
                                     fluidRow(column(3,
                                                     selectInput("variableEC",
                                                                  strong("Escolha uma opção:"),
                                                                  choices=c("Estado Civil"="EC",
                                                                            "Média de Idade por Estado Civil"="EC2"),
                                                                  selected = "EC")),
                                       column(9,
                                                     plotlyOutput("EC", height = 600)))),
                            ##
                            tabPanel("Cor/Raça",
                                     fluidRow(column(3,
                                                     selectInput("variableCor",
                                                                 strong("Escolha uma opção:"),
                                                                 choices=c("Cor/Raça"="Raca1",
                                                                           "Média de Idade por Raça"="Raca2"),
                                                                 selected = "Raca1")),
                                              column(9,
                                                     plotlyOutput("Raca", height = 600)))),

                            ##
                            tabPanel("Unidade da Federação",
                                     fluidRow(column(3,
                                                     selectInput("variableUF",
                                                                 strong("Escolha uma opção:"),
                                                                 choices=c("Candidatos por Estado"="UF2",
                                                                           "Proporção de Candidatos por Estado"="UF3"),
                                                                 selected = "UF2")),
                                              column(9,
                                                     plotlyOutput("UF1", height = 600)))),

                            ##
                            tabPanel("Ensino Médio",
                                     tabsetPanel(
                                       ##
                                       tabPanel("Situação",
                                                fluidRow(column(3,
                                                                selectInput("variableSit",
                                                                            strong("Escolha uma opção:"),
                                                                            choices=c("Situação de Conclusão"="Sit1",
                                                                                      "Média de Idade por Situação de Conclusão"="Sit2"),
                                                                            selected = "Sit1")),
                                                         column(9,
                                                                plotlyOutput("Sit", height = 600)))),

                                       ##
                                       tabPanel("Ano de Conclusão",
                                                fluidRow(column(3,
                                                                selectInput("variableAnno",
                                                                            strong("Escolha uma opção:"),
                                                                            choices=c("Ano de Conclusão"="Anno1",
                                                                                      "Média de Idade por Ano de Conclusão"="Anno2"),
                                                                            selected = "Anno1")),
                                                         column(9,
                                                                plotlyOutput("Anno", height = 600)))),

                                       ##
                                       tabPanel("Tipo de Escola",
                                                fluidRow(column(3,
                                                                selectInput("variableTipo",
                                                                            strong("Escolha uma opção:"),
                                                                            choices=c("Tipo de Escola"="Tipo1",
                                                                                      "Média de Idade por Tipo de Escola"="Tipo2"),
                                                                            selected = "Tipo1")),
                                                         column(9,
                                                                plotlyOutput("Tipo", height = 600))))

                                       ##
                                       #tabPanel("Tipo de Ensino",
                                       #         fluidRow(column(3,
                                       #                         selectInput("variableTip",
                                       #                                     strong("Escolha uma opção:"),
                                       #                                     choices=c("Tipo de Ensino"="Tip1",
                                       #                                               "Média de Idade por Tipo de Ensino"="Tip2"),
                                       #                                     selected = "Tip1")),
                                       #                  column(9,
                                       #                         plotlyOutput("Tip", height = 600))))
                                       #
                                       ##


                                     )# barra de navegacao interna
                            ),# barra de navegacao superior (Dados da Escola)



                            #tabPanel("Dados da Escola",
                            #         tabsetPanel(
                            #           ##
                            #           tabPanel("Depêndencia Administrativa",
                            #                    fluidRow(column(3,
                            #                                    selectInput("variableDadm",
                            #                                                strong("Escolha uma opção:"),
                            #                                                choices=c("Esfera"="Dadm1",
                            #                                                          "Média de Idade por Esfera da Escola"="Dadm2"),
                            #                                                selected = "Dadm1")),
                            #                             column(9,
                            #                                    plotlyOutput("Dadm", height = 600)))),
                            #
                            #           ##
                            #           tabPanel("Localização",
                            #                    fluidRow(column(3,
                            #                                    selectInput("variableLoc",
                            #                                                strong("Escolha uma opção:"),
                            #                                                choices=c("Localização"="Loc1",
                            #                                                          "Média de Idade por Localização"="Loc2"),
                            #                                                selected = "Loc1")),
                            #                             column(9,
                            #                                    plotlyOutput("Loc", height = 600))))
                            #
                            #           ##
                            #
                            #
                            #         )# barra de navegacao interna
                            #),# barra de navegacao superior (Dados da Escola)

                            tabPanel("Treemap",
                                     fluidRow(column(12,
                                                     plotOutput("treemap1", height = 600),
                                                     tableOutput("tabtreemap")))),
                            ##
                            tabPanel("Nacionalidade",
                                     fluidRow(column(3,
                                                     selectInput("variableNac",
                                                                 strong("Escolha uma opção:"),
                                                                 choices=c("Nacionalidade"="Nac1",
                                                                           "Média de Idade por Nacionalidade"="Nac2"),
                                                                 selected = "Nac1")),
                                              column(9,
                                                     plotlyOutput("Nac", height = 600)))),

                            ##
                            tabPanel("Treineiro",
                                     fluidRow(column(3,
                                                     selectInput("variableTrei",
                                                                 strong("Escolha uma opção:"),
                                                                 choices=c("Treineiro"="Trei1",
                                                                           "Média de Idade por Treineiro"="Trei2"),
                                                                 selected = "Trei1")),
                                              column(9,
                                                     plotlyOutput("Trei", height = 600))))

                            ##

                        )# barra de navegacao interna
                    ),# barra de navegacao superior (Dados do Participante)



#               tabPanel("DesempenhoAntigo",
#                        tabsetPanel(
#                          ##
#                          tabPanel("Desempenho dos Alunos no Enem com Base na Nota Final",
#                                   fluidRow(column(3,
#                                                   selectInput("variableLocc",
#                                                               strong("Escolha uma opção:"),
#                                                               choices=c("Localização"="Locc1",
#                                                                         "Média de Idade por Localização"="Locc2"),
#                                                               selected = "Locc1")),
#                                            column(9,
#                                                   plotlyOutput("Locc", height = 600))))
#
#                          ##
#
#
#
#
#
#                        )# barra de navegacao interna
#               ),# barra de navegacao superior (Prova Objetiva)

               #               tabPanel("Redação",
               #                        tabsetPanel(
               #                          ##
               #                          tabPanel("Situação",
               #                                   fluidRow(column(9,
               #                                                   plotlyOutput("SituationRED", height = 600)))),
               #                          tabPanel("Competência 1",
               #                                   fluidRow(column(9,
               #                                                   plotlyOutput("comp1", height = 600)))),
               #                          tabPanel("Competência 2",
               #                                   fluidRow(column(9,
               #                                                   plotlyOutput("comp2", height = 600)))),
               #                          tabPanel("Competência 3",
               #                                   fluidRow(column(9,
               #                                                   plotlyOutput("comp3", height = 600)))),
               #                          tabPanel("Competência 4",
               #                                   fluidRow(column(9,
               #                                                   plotlyOutput("comp4", height = 600)))),
               #                          tabPanel("Competência 5",
               #                                   fluidRow(column(9,
               #                                                   plotlyOutput("comp5", height = 600)))),
               #                          tabPanel("Redação",
               #                                   fluidRow(column(9,
               #                                                   plotlyOutput("NotaRedacao", height = 600))))
               #
               #
               #                        )# barra de navegacao interna
               #               ),# barra de navegacao superior (Redação)
               #














               tabPanel("Desempenho",
                        tabsetPanel(
                          ##
                          tabPanel("Ciências da Natureza",
                          #         fluidRow(column(9,
                          #                         plotlyOutput("CN", height = 600)))),



                          fluidRow(column(3,
                                          selectInput("variableCN",
                                                      strong("Escolha uma opção:"),
                                                      choices=c("Cor/Raça"="CN1",
                                                                "Gênero"="CN2",
                                                                "Estado Civil"="CN3",
                                                                "Unidade da Federação"="CN4",
                                                                "Treineiro"="CN5",
                                                                "Tipo de Escola"="CN6"),
                                                      selected = "CN1")),
                                   column(9,
                                          plotlyOutput("CN", height = 600),
                                          tableOutput("CNQ027")))),
                        ##


                          tabPanel("Ciências Humanas",
                                   fluidRow(column(3,
                                                   selectInput("variableCH",
                                                               strong("Escolha uma opção:"),
                                                               choices=c("Cor/Raça"="CH1",
                                                                         "Gênero"="CH2",
                                                                         "Estado Civil"="CH3",
                                                                         "Unidade da Federação"="CH4",
                                                                         "Treineiro"="CH5",
                                                                         "Tipo de Escola"="CH6"),
                                                               selected = "CH1")),
                                            column(9,
                                                   plotlyOutput("CH", height = 600),
                                                   tableOutput("CHQ027")))),
                        ##


                        tabPanel("Linguagens e Códigos",
                                   fluidRow(column(3,
                                                   selectInput("variableLC",
                                                               strong("Escolha uma opção:"),
                                                               choices=c("Cor/Raça"="LC1",
                                                                         "Gênero"="LC2",
                                                                         "Estado Civil"="LC3",
                                                                         "Unidade da Federação"="LC4",
                                                                         "Treineiro"="LC5",
                                                                         "Tipo de Escola"="LC6"),
                                                               selected = "LC1")),
                                            column(9,
                                                   plotlyOutput("LC", height = 600),
                                                   tableOutput("LCQ027")))),
                        ##


                          tabPanel("Matemática",
                                   fluidRow(column(3,
                                                   selectInput("variableMT",
                                                               strong("Escolha uma opção:"),
                                                               choices=c("Cor/Raça"="MT1",
                                                                         "Gênero"="MT2",
                                                                         "Estado Civil"="MT3",
                                                                         "Unidade da Federação"="MT4",
                                                                         "Treineiro"="MT5",
                                                                         "Tipo de Escola"="MT6"),
                                                               selected = "MT1")),
                                            column(9,
                                                   plotlyOutput("MT", height = 600),
                                                   tableOutput("MTQ027")))),
                        ##


                          tabPanel("Redação",
                                   tabsetPanel(
                                     ##
                                     tabPanel("Situação",
                                              fluidRow(column(9,
                                                              plotlyOutput("SituationRED", height = 600),
                                                              tableOutput("REDSIT")))),
                                     tabPanel("Competência 1",
                                              fluidRow(column(3,
                                                              selectInput("variableCOMP1",
                                                                          strong("Escolha uma opção:"),
                                                                          choices=c("Cor/Raça"="COMP11",
                                                                                    "Gênero"="COMP12",
                                                                                    "Estado Civil"="COMP13",
                                                                                    "Unidade da Federação"="COMP14",
                                                                                    "Treineiro"="COMP15",
                                                                                    "Tipo de Escola"="COMP16"),
                                                                          selected = "COMP11")),
                                                       column(9,
                                                              plotlyOutput("COMP1", height = 600),
                                                              tableOutput("COMP1Q027"))),
                                                              strong("*Observação: A Competência 1 avalia a capacidade do candidato demonstrar domínio da modalidade escrita formal da Língua Portuguesa. A nota máxima possível de ser obtida nesta Competência é de 200 pontos.")),

                                     ##
                                     tabPanel("Competência 2",
                                              fluidRow(column(3,
                                                              selectInput("variableCOMP2",
                                                                          strong("Escolha uma opção:"),
                                                                          choices=c("Cor/Raça"="COMP21",
                                                                                    "Gênero"="COMP22",
                                                                                    "Estado Civil"="COMP23",
                                                                                    "Unidade da Federação"="COMP24",
                                                                                    "Treineiro"="COMP25",
                                                                                    "Tipo de Escola"="COMP26"),
                                                                          selected = "COMP21")),
                                                       column(9,
                                                              plotlyOutput("COMP2", height = 600),
                                                              tableOutput("COMP2Q027"))),
                                                              strong("*Observação: A Competência 2 avalia a capacidade do candidato compreender a proposta de redação e aplicar conceitos das várias áreas de conhecimento para desenvolver o tema, dentro dos limites estruturais do texto dissertativo-argumentativo em prosa. A nota máxima possível de ser obtida nesta Competência é de 200 pontos.")),

                                     ##
                                     tabPanel("Competência 3",
                                              fluidRow(column(3,
                                                              selectInput("variableCOMP3",
                                                                          strong("Escolha uma opção:"),
                                                                          choices=c("Cor/Raça"="COMP31",
                                                                                    "Gênero"="COMP32",
                                                                                    "Estado Civil"="COMP33",
                                                                                    "Unidade da Federação"="COMP34",
                                                                                    "Treineiro"="COMP35",
                                                                                    "Tipo de Escola"="COMP36"),
                                                                          selected = "COMP31")),
                                                       column(9,
                                                              plotlyOutput("COMP3", height = 600),
                                                              tableOutput("COMP3Q027"))),
                                                              strong("Observação: A Competência 3 avalia a capacidade do candidato selecionar, relacionar, organizar e interpretar informações, fatos, opiniões e argumentos em defesa de um ponto de vista.A nota máxima possível de ser obtida nesta Competência é de 200 pontos.")),

                                     ##
                                     tabPanel("Competência 4",
                                              fluidRow(column(3,
                                                              selectInput("variableCOMP4",
                                                                          strong("Escolha uma opção:"),
                                                                          choices=c("Cor/Raça"="COMP41",
                                                                                    "Gênero"="COMP42",
                                                                                    "Estado Civil"="COMP43",
                                                                                    "Unidade da Federação"="COMP44",
                                                                                    "Treineiro"="COMP45",
                                                                                    "Tipo de Escola"="COMP46"),
                                                                          selected = "COMP41")),
                                                       column(9,
                                                              plotlyOutput("COMP4", height = 600),
                                                              tableOutput("COMP4Q027"))),
                                                              strong("Observação: A Competência 4 avalia a capacidade do candidato demonstrar conhecimento dos mecanismos linguísticos necessários para a construção da argumentação. A nota máxima possível de ser obtida nesta Competência é de 200 pontos.")),

                                     ##
                                     tabPanel("Competência 5",
                                              fluidRow(column(3,
                                                              selectInput("variableCOMP5",
                                                                          strong("Escolha uma opção:"),
                                                                          choices=c("Cor/Raça"="COMP51",
                                                                                    "Gênero"="COMP52",
                                                                                    "Estado Civil"="COMP53",
                                                                                    "Unidade da Federação"="COMP54",
                                                                                    "Treineiro"="COMP55",
                                                                                    "Tipo de Escola"="COMP56"),
                                                                          selected = "COMP51")),
                                                       column(9,
                                                              plotlyOutput("COMP5", height = 600),
                                                              tableOutput("COMP5Q027"))),
                                                              strong("Observação: A Competência 5 avalia a capacidade do candidato elaborar proposta de intervenção para o problema abordado, respeitando os direitos humanos. A nota máxima possível de ser obtida nesta Competência é de 200 pontos.")),

                                     ##
                                     tabPanel("Nota Redação",
                                              fluidRow(column(3,
                                                              selectInput("variableRE",
                                                                          strong("Escolha uma opção:"),
                                                                          choices=c("Cor/Raça"="RE1",
                                                                                    "Gênero"="RE2",
                                                                                    "Estado Civil"="RE3",
                                                                                    "Unidade da Federação"="RE4",
                                                                                    "Treineiro"="RE5",
                                                                                    "Tipo de Escola"="RE6"),
                                                                          selected = "RE1")),
                                                       column(9,
                                                              plotlyOutput("RE", height = 600),
                                                              tableOutput("REQ027"))),
                                                              strong("Observação: A nota final da redação consiste na soma das notas das 5 Competências avaliadas. Assim, a nota máxima possível de ser obtida na redação é de 1.000 pontos."))

                                     ##


                                   )# barra de navegacao interna
                          ),# barra de navegacao superior (Redação)

                        tabPanel("Nota Final",
                                 fluidRow(column(3,
                                                 selectInput("variableNF",
                                                             strong("Escolha uma opção:"),
                                                             choices=c("Cor/Raça"="NF1",
                                                                       "Gênero"="NF2",
                                                                       "Estado Civil"="NF3",
                                                                       "Unidade da Federação"="NF4",
                                                                       "Treineiro"="NF5",
                                                                       "Tipo de Escola"="NF6"),
                                                             selected = "NF1")),

                                          column(9,
                                                 plotlyOutput("NF", height = 600),
                                                 tableOutput("NFQ027"))))
                        ##


                        )# barra de navegacao interna
               ),# barra de navegacao superior (Prova Objetiva)

#               tabPanel("Redação",
#                        tabsetPanel(
#                          ##
#                          tabPanel("Situação",
#                                   fluidRow(column(9,
#                                                   plotlyOutput("SituationRED", height = 600)))),
#                          tabPanel("Competência 1",
#                                   fluidRow(column(9,
#                                                   plotlyOutput("comp1", height = 600)))),
#                          tabPanel("Competência 2",
#                                   fluidRow(column(9,
#                                                   plotlyOutput("comp2", height = 600)))),
#                          tabPanel("Competência 3",
#                                   fluidRow(column(9,
#                                                   plotlyOutput("comp3", height = 600)))),
#                          tabPanel("Competência 4",
#                                   fluidRow(column(9,
#                                                   plotlyOutput("comp4", height = 600)))),
#                          tabPanel("Competência 5",
#                                   fluidRow(column(9,
#                                                   plotlyOutput("comp5", height = 600)))),
#                          tabPanel("Redação",
#                                   fluidRow(column(9,
#                                                   plotlyOutput("NotaRedacao", height = 600))))
#
#
#                        )# barra de navegacao interna
#               ),# barra de navegacao superior (Redação)
#
               tabPanel("Questionário Socioeconômico",
                        tabsetPanel(
                          ##
                          tabPanel("Por Quantidade de Candidatos",
                                   fluidRow(column(3,
                                                   selectInput("variableQ",
                                                               strong("Escolha uma opção:"),
                                                               choices=c("Q001"="Q001",
                                                                         "Q002"="Q002",
                                                                         "Q003"="Q003",
                                                                         "Q004"="Q004",
                                                                         "Q005"="Q005",
                                                                         "Q006"="Q006",
                                                                         "Q007"="Q007",
                                                                         "Q008"="Q008",
                                                                         "Q009"="Q009",
                                                                         "Q010"="Q010",
                                                                         "Q011"="Q011",
                                                                         "Q012"="Q012",
                                                                         "Q013"="Q013",
                                                                         "Q014"="Q014",
                                                                         "Q015"="Q015",
                                                                         "Q016"="Q016",
                                                                         "Q017"="Q017",
                                                                         "Q018"="Q018",
                                                                         "Q019"="Q019",
                                                                         "Q020"="Q020",
                                                                         "Q021"="Q021",
                                                                         "Q022"="Q022",
                                                                         "Q023"="Q023",
                                                                         "Q024"="Q024",
                                                                         "Q025"="Q025",
                                                                         "Q026"="Q026",
                                                                         "Q027"="Q027"),
                                                               selected = "Q001")),
                                            column(9,
                                                   textOutput("TextQ00"),
                                                   plotlyOutput("Q00", height = 400),
                                                   tableOutput("tabQ001")))),

                            ##
                          tabPanel("Por Nota Média",
                                   fluidRow(column(3,
                                                   selectInput("variableQ2",
                                                               strong("Escolha uma opção:"),
                                                               choices=c("Q001"="Q001",
                                                                         "Q002"="Q002",
                                                                         "Q003"="Q003",
                                                                         "Q004"="Q004",
                                                                         "Q005"="Q005",
                                                                         "Q006"="Q006",
                                                                         "Q007"="Q007",
                                                                         "Q008"="Q008",
                                                                         "Q009"="Q009",
                                                                         "Q010"="Q010",
                                                                         "Q011"="Q011",
                                                                         "Q012"="Q012",
                                                                         "Q013"="Q013",
                                                                         "Q014"="Q014",
                                                                         "Q015"="Q015",
                                                                         "Q016"="Q016",
                                                                         "Q017"="Q017",
                                                                         "Q018"="Q018",
                                                                         "Q019"="Q019",
                                                                         "Q020"="Q020",
                                                                         "Q021"="Q021",
                                                                         "Q022"="Q022",
                                                                         "Q023"="Q023",
                                                                         "Q024"="Q024",
                                                                         "Q025"="Q025",
                                                                         "Q026"="Q026",
                                                                         "Q027"="Q027"),
                                                               selected = "Q001")),
                                            column(9,
                                                   textOutput("TextQ00222"),
                                                   plotlyOutput("Q00222", height = 400),
                                                   tableOutput("tabQ001222"))))

                          ##
#                            tabPanel("Por Nota MédiaX",
                                     #         fluidRow(column(9,
                                     #                         plotlyOutput("CN", height = 600)))),



#                                     fluidRow(column(3,
#                                                     selectInput("variableD",
#                                                                 strong("Escolha uma opção:"),
#                                                                 choices=c("Cor/Raça"="D1",
#                                                                           "Gênero"="D2",
#                                                                           "Estado Civil"="D3",
#                                                                           "Unidade da Federação"="D4",
#                                                                           "Escolaridade da Mãe"="D5",
#                                                                           "Escolaridade do Pai"="D6",
#                                                                           "Ocupação da Mãe"="D7",
#                                                                           "Ocupação do Pai"="D8",
#                                                                           "Tipo de Escola"="D9",
#                                                                           "Renda da Família"="D10",
#                                                                           "Treineiro"="D11"),
#
#                                                                 selected = "D1")),
#                                              column(9,
#                                                     plotlyOutput("D", height = 600))))

                          ##








                        )# barra de navegacao interna
               )# barra de navegacao superior (SocioEconomico)

        )#navbarPage
    )#fluidPage
)#shinyUI
