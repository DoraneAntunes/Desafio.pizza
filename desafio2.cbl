      *Divisão de identificação do programa
       identification division.
       program-id. "desafio2".
       author. " Dorane Antunes".
       installation. "PC".
       date-written. 08/07/2020.
       date-compiled. 08/07/2020.



      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

       01  relatorio  occurs  20.
           05 nome                                 pic x(15).
           05 filler                               pic x(03)
              value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 dif_rel                              pic 9(03)V9(02).
           05 filler                               pic x(03)
              value " - ".
           05 piz_a                                pic 9(03)V99.

       01  aux.
           05 nome_aux                             pic x(15).
           05 filler                               pic x(03)
              value " - ".
           05 diametro_aux                         pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 preco_aux                            pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 preco_cm2_aux                        pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 dif_rel_aux                          pic 9(03)V9(02).
           05 filler                               pic x(03)
              value " - ".
           05 piz_a_aux                            pic 9(03)V99.

       77 dif                                      pic 9(02)V99.
       77 ind                                      pic 9(02).
       77 menu                                     pic x(01).
       77 pi                                       pic 9(01)V9(06)
          value 3,141592.
       77 controle                                 pic X(20).


      *----Variaveis para comunicação entre programas
       linkage section.


      *----Declaração de tela
       screen section.


      *Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.
           move 'S'         to menu
           move 'trocou'    to controle
           move 0           to ind
           .
       inicializa-exit.
           exit.


       processamento section.
               move 0 to ind
           perform until menu = 'N' or menu = 'n'
               display erase
               add 1 to ind

               if ind > 20 then
                   display "Voce atingiu o limite de 20 pizzas"
               else
                   display "Informe o nome da pizza: "
                   accept nome(ind)

                   display "Informe o diametro: "
                   accept diametro(ind)

                   display "Informe o preco: "
                   accept preco(ind)
               end-if

               compute piz_a(ind) = pi*(diametro(ind)/2)**2

               compute preco_cm2(ind) = preco(ind)/ piz_a(ind)


              display "deseja cadastrar mais uma pizza? ('S'/'N')"
               accept menu

           end-perform

           perform ordenacao.
           perform diferenca.
           perform tela.

           perform varying ind from 1 by 1 until ind > 20
                                              or nome(ind) = space
              display relatorio(ind)
           end-perform


           .
       processamento-exit.
           exit.

       ordenacao section.
      * ordenaçao das pizzas em ordem de cm2

           move 'trocou' to controle
           perform until controle <> 'trocou'

               move 1 to ind
               move 'n_trocou' to controle

               perform until ind = 20
                       or nome(ind + 1) = space

                   if preco_cm2(ind) > preco_cm2(ind + 1) then
                   move relatorio(ind + 1)  to aux
                   move relatorio(ind)      to relatorio(ind + 1)
                   move aux                 to relatorio(ind)

                   move 'trocou'          to controle
                   end-if
                   add 1 to ind
               end-perform
           end-perform

           .
       ordenacao-exit.
       exit.
      *-----------------------------------------------------------------
       diferenca section.

           move 1 to ind
           perform until ind = 20
                          or nome(ind + 1) = space

               compute dif = preco_cm2(ind + 1) - preco_cm2(ind)

               compute dif_rel(ind + 1)= dif * 100 / preco_cm2(ind)
               add 1 to ind
           end-perform
           .
       diferenca-exit.
       exit.
      *-----------------------------------------------------------------
       tela section.

           display " "
           display "Nome: " at 1001
           display " "
           display "Diametro: " at 1015
           display " "
           display "Preco: " at 1025
           display " "
           display "R$ cm2: " at 1032
           display " "
           display "Porcentagem: " at 1042
           display " "

           .
       tela-exit.
       exit.
      *-----------------------------------------------------------------
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.













