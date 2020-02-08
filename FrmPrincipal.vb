Public Class FrmPrincipal
    Dim r As New System.Random(System.DateTime.Now.Millisecond) ' verdadero random
    Dim thisbutton(81) As Button ' declarar 81 botones

    Dim thisEjbtn(3) As Button
    Public correct, incorrect1, incorrect2 As String

    Public AcontadorS As Integer
    Public seVioEjercicioAtras As Boolean = False
    Public seVioEjercicioAdelante As Boolean = False

    Private SolutionMostrada As Boolean = False 'muestra o no muestra la solucion

    Private SolucionSudoku(81) As String 'solucion total
    Private SudokuUsuario(81) As String 'solucion usuario

    Private Sub FrmPrincipal_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'conectar botones de forms con boton de array de botones respectivamente
        TxtGano.Visible = False
        '
        chkCorrecto.Enabled = False
        TxtGano.Enabled = False
        TxtPass.Enabled = False
        BtnRespuesta1.Enabled = False
        BtnRespuesta2.Enabled = False
        BtnRespuesta3.Enabled = False
        '
        thisEjbtn(1) = BtnRespuesta1
        thisEjbtn(2) = BtnRespuesta2
        thisEjbtn(3) = BtnRespuesta3
        '
        thisbutton(1) = Button1
        thisbutton(2) = Button2
        thisbutton(3) = Button3
        thisbutton(4) = Button4
        thisbutton(5) = Button5
        thisbutton(6) = Button6
        thisbutton(7) = Button7
        thisbutton(8) = Button8
        thisbutton(9) = Button9
        thisbutton(10) = Button10
        thisbutton(11) = Button11
        thisbutton(12) = Button12
        thisbutton(13) = Button13
        thisbutton(14) = Button14
        thisbutton(15) = Button15
        thisbutton(16) = Button16
        thisbutton(17) = Button17
        thisbutton(18) = Button18
        thisbutton(19) = Button19
        thisbutton(20) = Button20
        thisbutton(21) = Button21
        thisbutton(22) = Button22
        thisbutton(23) = Button23
        thisbutton(24) = Button24
        thisbutton(25) = Button25
        thisbutton(26) = Button26
        thisbutton(27) = Button27
        thisbutton(28) = Button28
        thisbutton(29) = Button29
        thisbutton(30) = Button30
        thisbutton(31) = Button31
        thisbutton(32) = Button32
        thisbutton(33) = Button33
        thisbutton(34) = Button34
        thisbutton(35) = Button35
        thisbutton(36) = Button36
        thisbutton(37) = Button37
        thisbutton(38) = Button38
        thisbutton(39) = Button39
        thisbutton(40) = Button40
        thisbutton(41) = Button41
        thisbutton(42) = Button42
        thisbutton(43) = Button43
        thisbutton(44) = Button44
        thisbutton(45) = Button45
        thisbutton(46) = Button46
        thisbutton(47) = Button47
        thisbutton(48) = Button48
        thisbutton(49) = Button49
        thisbutton(50) = Button50
        thisbutton(51) = Button51
        thisbutton(52) = Button52
        thisbutton(53) = Button53
        thisbutton(54) = Button54
        thisbutton(55) = Button55
        thisbutton(56) = Button56
        thisbutton(57) = Button57
        thisbutton(58) = Button58
        thisbutton(59) = Button59
        thisbutton(60) = Button60
        thisbutton(61) = Button61
        thisbutton(62) = Button62
        thisbutton(63) = Button63
        thisbutton(64) = Button64
        thisbutton(65) = Button65
        thisbutton(66) = Button66
        thisbutton(67) = Button67
        thisbutton(68) = Button68
        thisbutton(69) = Button69
        thisbutton(70) = Button70
        thisbutton(71) = Button71
        thisbutton(72) = Button72
        thisbutton(73) = Button73
        thisbutton(74) = Button74
        thisbutton(75) = Button75
        thisbutton(76) = Button76
        thisbutton(77) = Button77
        thisbutton(78) = Button78
        thisbutton(79) = Button79
        thisbutton(80) = Button80
        thisbutton(81) = Button81
        TxtGano.Visible = False
        Dim i As Byte
        For i = 1 To 81 'Botones desactivados antes de crear el marco
            thisbutton(i).Enabled = False
        Next
        BtnSolucion.Enabled = False 'aun no existe solucion ya que no se creo el marco
    End Sub
    Private Sub BtnNuevo_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnNuevo.Click
        TxtGano.Visible = False
        While Not Crear_Marco() 'mientras Crear_Marco() sea igual a falso, en proceso de creacion
            NUDDificultad.Enabled = False 'no se puede usar el NUD para cambiar la dificultad
            TxtPass.Enabled = False
            BtnSolucion.Enabled = False
        End While
        TxtPass.Enabled = True
        NUDDificultad.Enabled = True 'cuando Crear_Marco() sea igual a true, ya se creo
        BtnSolucion.Enabled = True 'se puede botonear a mostrar la solucion
        SolutionMostrada = False 'aun no mostrada la solucion
    End Sub
    Private Function Crear_Marco() As Boolean
        Dim i, j, k As Byte
        Dim aleatorio As Byte
        Dim botonActual As Button
        Dim valorAcepta(9) As Boolean
        Dim valorOK As Byte = 0
        For i = 1 To 81 'LIMPIAR 81 botones
            botonActual = thisbutton(i)
            botonActual.Text = ""
            botonActual.BackColor = System.Drawing.Color.LightGreen 'verde es los fijos
            SolucionSudoku(i) = ""
        Next
        For i = 1 To 81 'cada boton con respectiva ubicacion
            botonActual = thisbutton(i)
            valorOK = ObtenerValoresValidos(i, valorAcepta) 'returned: acumulacion de bienes
            If valorOK = 0 Then ' NO SE PUEDE RESOLVER (puro males), OTRA VEZ CLICK EN BOTON 'Nuevo'
                Return False ' no se puede botonear solucion y marco vacio
            End If
            ' SE PUEDE RESOLVER
            aleatorio = r.Next(0, valorOK) + 1 ' "+1" descluye 1er termino, incluye 2do termino ; desde el 1 al numero de bienes que acepta
            '
            k = 0
            For j = 1 To 9 ' 9(males+bienes) por ubicacion ; 729(males+bienes) total(81 ubicaciones) ; va ser un numero desde el 1 al 9 la ubicacion actual
                If valorAcepta(j) = True Then ' acepta solo los bienes por ubicacion
                    k = k + 1 'counter de bienes por ubicacion
                    If k = aleatorio Then ' cuando 'k'(max- hasta numero de bienes) llegue a ser el numero aleatorio aceptado, salgase
                        Exit For
                    End If
                End If
            Next
            '
            botonActual.Text = j.ToString() 'y asigne a la ubicacion actual el j que es aceptado como un bien
        Next
        ' MARCO LLENO DE VALORES---------------
        ' FASE 2: ASIGNACION INTELIGENTE(RANDOMICA) AL MARCO PARA PODER RESOLVER 
        For i = 1 To 81
            botonActual = thisbutton(i)
            SolucionSudoku(i) = botonActual.Text 'SOLUCION FINAL
            aleatorio = r.Next(0, NUDDificultad.Value + 1) + 1 ' "+1" descluye 1er termino, incluye 2do termino
            If aleatorio > 1 Then ' entre mas espacios de numeros mayor que 1, mas dificil
                botonActual.Text = "" ' vacio para resolver
                botonActual.BackColor = System.Drawing.Color.Orange ' naranja para resolver
                botonActual.Enabled = True ' boton naranja activada
            Else ' no poder resolver boton actual verde
                botonActual.Enabled = False ' verde desactivado
            End If
        Next
        Return True ' marco aceptado y terminado, turno del usuario...
    End Function
    Private Function ObtenerValoresValidos(ByVal ubicacion As Integer, ByRef ValoresValidos As Boolean()) As Integer
        Dim i, j, k As Byte
        Dim fila, columna As Byte
        Dim numAString As String
        Dim numEsOK As Boolean
        Dim valorRetornado As Byte = 0
        For i = 1 To 9 ' 9 veces por ubicacion ; 729 veces total
            numEsOK = True
            numAString = i.ToString()
            If numEsOK = True Then 'FILA ; izquierda a derecha
                For j = 1 To 9
                    k = (Int((ubicacion - 1) / 9) * 9) + j ' botones de la fila
                    If thisbutton(k).Text = numAString Then ' botones tiene string 1-9?
                        numEsOK = False ' mal
                        Exit For
                    End If
                Next
            End If
            If numEsOK = True Then 'COLUMNA ; arriba->abajo
                For j = 1 To 9
                    k = (Int((ubicacion - 1) Mod 9) + 1) + ((j - 1) * 9) ' botones de la columna
                    If thisbutton(k).Text = numAString Then ' botones tiene string 1-9?
                        numEsOK = False ' mal
                        Exit For
                    End If
                Next
            End If
            If numEsOK = True Then 'CUADRANTE ; por fila-columnas
                fila = Int(Int((ubicacion - 1) / 9) / 3)
                columna = Int(Int((ubicacion - 1) Mod 9) / 3)
                For j = 1 To 9
                    k = (((fila * 3) + Int((j - 1) / 3)) * 9) + (columna * 3) + Int((j - 1) Mod 3) + 1 ' botones del cuadrante
                    If thisbutton(k).Text = numAString Then ' botones tiene string 1-9?
                        numEsOK = False ' mal
                        Exit For
                    End If
                Next
            End If
            If numEsOK = True Then ' bien
                valorRetornado = valorRetornado + 1 ' acumulacion de bienes (0-9) ; El NUMERO MAXIMO ACEPTADO
            End If
            ValoresValidos(i) = numEsOK ' para cada ubicacion, acumula 9 males o bienes 
        Next
        Return valorRetornado ' retorna bienes acumulados ; HASTA EL NUMERO MAXIMO ACEPTADO
    End Function
    Private Function correctoOIncorrecto() As Nullable(Of Integer)
        chkCorrecto.Enabled = False
        If chkCorrecto.Checked = False Then
            For i = 1 To 81
                If (thisbutton(i).BackColor = Color.Gray) Then
                    thisbutton(i).BackColor = Color.Orange
                    thisbutton(i).Text = "1"
                End If
            Next
        Else
            For i = 1 To 81
                If (thisbutton(i).BackColor = Color.Gray) Then
                    thisbutton(i).Enabled = False
                    thisbutton(i).BackColor = Color.HotPink
                    thisbutton(i).Text = SolucionSudoku(i)
                End If
            Next
        End If
    End Function
    Private Function alAzarBotonYEjercicio(correct As String, incorrect1 As String, incorrect2 As String) As Nullable(Of Integer)
        Dim RespuestaCorrectaBtn As Byte
        Dim incorrectoRandom As Byte
        RespuestaCorrectaBtn = r.Next(0, 3) + 1
        Select Case RespuestaCorrectaBtn
            Case 1
                BtnRespuesta1.Text = correct
                incorrectoRandom = r.Next(0, 2) + 1
                Select Case incorrectoRandom
                    Case 1
                        BtnRespuesta2.Text = incorrect1
                        BtnRespuesta3.Text = incorrect2
                    Case 2
                        BtnRespuesta2.Text = incorrect2
                        BtnRespuesta3.Text = incorrect1
                End Select
            Case 2
                BtnRespuesta2.Text = correct
                incorrectoRandom = r.Next(0, 2) + 1
                Select Case incorrectoRandom
                    Case 1
                        BtnRespuesta1.Text = incorrect1
                        BtnRespuesta3.Text = incorrect2
                    Case 2
                        BtnRespuesta1.Text = incorrect2
                        BtnRespuesta3.Text = incorrect1
                End Select
            Case 3
                BtnRespuesta3.Text = correct
                incorrectoRandom = r.Next(0, 2) + 1
                Select Case incorrectoRandom
                    Case 1
                        BtnRespuesta1.Text = incorrect1
                        BtnRespuesta2.Text = incorrect2
                    Case 2
                        BtnRespuesta1.Text = incorrect2
                        BtnRespuesta2.Text = incorrect1
                End Select
        End Select
    End Function
    Private Sub Btns_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click, Button2.Click, Button3.Click, Button4.Click, Button5.Click, Button6.Click, Button7.Click, Button8.Click, Button9.Click, Button10.Click, Button11.Click, Button12.Click, Button13.Click, Button14.Click, Button15.Click, Button16.Click, Button17.Click, Button18.Click, Button19.Click, Button20.Click, Button21.Click, Button22.Click, Button23.Click, Button24.Click, Button25.Click, Button26.Click, Button27.Click, Button28.Click, Button29.Click, Button30.Click, Button31.Click, Button32.Click, Button33.Click, Button34.Click, Button35.Click, Button36.Click, Button37.Click, Button38.Click, Button39.Click, Button40.Click, Button41.Click, Button42.Click, Button43.Click, Button44.Click, Button45.Click, Button46.Click, Button47.Click, Button48.Click, Button49.Click, Button50.Click, Button51.Click, Button52.Click, Button53.Click, Button54.Click, Button55.Click, Button56.Click, Button57.Click, Button58.Click, Button59.Click, Button60.Click, Button61.Click, Button62.Click, Button63.Click, Button64.Click, Button65.Click, Button66.Click, Button67.Click, Button68.Click, Button69.Click, Button70.Click, Button71.Click, Button72.Click, Button73.Click, Button74.Click, Button75.Click, Button76.Click, Button77.Click, Button78.Click, Button79.Click, Button80.Click, Button81.Click
        Dim i As Byte
        Dim encontradaSolucion = False
        Dim contenedorGray As Byte = 0
        Dim btnActual As Button = CType(sender, Button)
        If btnActual.Text = "" Then ' para cada botoncito tener habilidad de incrementar su numero actual
            chkCorrecto.Checked = False
            BtnAnterior.Enabled = False
            BtnSiguiente.Enabled = False
            For i = 1 To 3
                thisEjbtn(i).BackColor = Color.Chocolate
                thisEjbtn(i).Enabled = True
            Next
            For i = 1 To 81
                If thisbutton(i).BackColor = Color.Gray Then
                    contenedorGray = contenedorGray + 1
                End If
            Next
            If contenedorGray = 0 Then
                If seVioEjercicioAdelante = True Or seVioEjercicioAtras = True Then
                    seVioEjercicioAdelante = False
                    seVioEjercicioAtras = False
                ElseIf (seVioEjercicioAdelante = False And seVioEjercicioAtras = False) Then
                    If (AcontadorS = 81) Then
                        AcontadorS = 1
                    Else
                        AcontadorS = AcontadorS + 1
                    End If
                End If
                Select Case AcontadorS
                    Case 1
                        PcbEjercicio.Image = My.Resources._1
                        correct = "T=6"
                        incorrect1 = "T=" & (r.Next(-5, 5) + 1)
                        incorrect2 = "T=" & (r.Next(6, 16) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 2
                        PcbEjercicio.Image = My.Resources._2
                        correct = "T=9"
                        incorrect1 = "T=" & (r.Next(-2, 8) + 1)
                        incorrect2 = "T=" & (r.Next(9, 19) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 3
                        PcbEjercicio.Image = My.Resources._3
                        correct = 3
                        incorrect1 = r.Next(-8, 2) + 1
                        incorrect2 = r.Next(3, 13) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 4
                        PcbEjercicio.Image = My.Resources._4
                        correct = "T=3"
                        incorrect1 = "T=" & (r.Next(-8, 2) + 1)
                        incorrect2 = "T=" & (r.Next(3, 13) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 5
                        PcbEjercicio.Image = My.Resources._5
                        correct = 5
                        incorrect1 = r.Next(-6, 4) + 1
                        incorrect2 = r.Next(5, 15) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 6
                        PcbEjercicio.Image = My.Resources._6
                        correct = "T=2"
                        incorrect1 = "T=" & (r.Next(-9, 1) + 1)
                        incorrect2 = "T=" & (r.Next(2, 12) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 7
                        PcbEjercicio.Image = My.Resources._7
                        correct = "T=4"
                        incorrect1 = "T=" & (r.Next(-6, 4) + 1)
                        incorrect2 = "T=" & (r.Next(4, 14) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 8
                        PcbEjercicio.Image = My.Resources._8
                        correct = "T=7"
                        incorrect1 = "T=" & (r.Next(-4, 6) + 1)
                        incorrect2 = "T=" & (r.Next(7, 17) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 9
                        PcbEjercicio.Image = My.Resources._9
                        correct = 2
                        incorrect1 = r.Next(-9, 1) + 1
                        incorrect2 = r.Next(2, 12) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 10
                        PcbEjercicio.Image = My.Resources._10
                        correct = 8
                        incorrect1 = r.Next(-3, 7) + 1
                        incorrect2 = r.Next(8, 18) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 11
                        PcbEjercicio.Image = My.Resources._11
                        correct = "T=9"
                        incorrect1 = "T=" & (r.Next(-2, 8) + 1)
                        incorrect2 = "T=" & (r.Next(9, 19) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 12
                        PcbEjercicio.Image = My.Resources._12
                        correct = "T=1"
                        incorrect1 = "T=" & (r.Next(-10, 0) + 1)
                        incorrect2 = "T=" & (r.Next(1, 10) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 13
                        PcbEjercicio.Image = My.Resources._13
                        correct = "T=4"
                        incorrect1 = "T=" & (r.Next(-7, 3) + 1)
                        incorrect2 = "T=" & (r.Next(4, 14) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 14
                        PcbEjercicio.Image = My.Resources._14
                        correct = "T=6"
                        incorrect1 = "T=" & (r.Next(-5, 5) + 1)
                        incorrect2 = "T=" & (r.Next(6, 16) + 1)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 15
                        PcbEjercicio.Image = My.Resources._15
                        correct = 4
                        incorrect1 = r.Next(-7, 3) + 1
                        incorrect2 = r.Next(4, 14) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 16
                        PcbEjercicio.Image = My.Resources._16
                        correct = 8
                        incorrect1 = r.Next(-3, 7) + 1
                        incorrect2 = r.Next(8, 18) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 17
                        PcbEjercicio.Image = My.Resources._17
                        correct = 2
                        incorrect1 = r.Next(-9, 1) + 1
                        incorrect2 = r.Next(2, 12) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 18
                        PcbEjercicio.Image = My.Resources._18
                        correct = 8
                        incorrect1 = r.Next(-3, 7) + 1
                        incorrect2 = r.Next(8, 18) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 19
                        PcbEjercicio.Image = My.Resources._19
                        correct = 1
                        incorrect1 = r.Next(-10, 0) + 1
                        incorrect2 = r.Next(1, 10) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 20
                        PcbEjercicio.Image = My.Resources._20
                        correct = 1
                        incorrect1 = r.Next(-10, 0) + 1
                        incorrect2 = r.Next(1, 10) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 21
                        PcbEjercicio.Image = My.Resources._21
                        correct = "5; 3"
                        incorrect1 = r.Next(-6, 4) + 1 & "; " & r.Next(-8, 2) + 1
                        incorrect2 = r.Next(5, 15) + 1 & "; " & r.Next(3, 13) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 22
                        PcbEjercicio.Image = My.Resources._22
                        correct = "2; 4"
                        incorrect1 = r.Next(-9, 1) + 1 & "; " & r.Next(-7, 3) + 1
                        incorrect2 = r.Next(2, 12) + 1 & "; " & r.Next(4, 14) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 23
                        PcbEjercicio.Image = My.Resources._23
                        correct = 5
                        incorrect1 = r.Next(-6, 4) + 1
                        incorrect2 = r.Next(5, 15) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 24
                        PcbEjercicio.Image = My.Resources._24
                        correct = "2; -2"
                        incorrect1 = r.Next(-9, 1) + 1 & "; " & r.Next(-7, -3) + 1
                        incorrect2 = r.Next(2, 12) + 1 & "; " & r.Next(-2, 8) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 25
                        PcbEjercicio.Image = My.Resources._25
                        correct = "8; 7.676,43"
                        incorrect1 = r.Next(-3, 7) + 1 & "; " & r.Next(-3, 7) + 1
                        incorrect2 = r.Next(8, 18) + 1 & "; " & r.Next(8, 18) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 26
                        PcbEjercicio.Image = My.Resources._26
                        correct = "416; 96; 276"
                        incorrect1 = r.Next(405, 415) + 1 & "; " & r.Next(85, 95) + 1 & "; " & r.Next(265, 275)
                        incorrect2 = r.Next(416, 426) + 1 & "; " & r.Next(96, 106) + 1 & "; " & r.Next(276, 286)
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 27
                        PcbEjercicio.Image = My.Resources._27
                        correct = "32,5; 1,8"
                        incorrect1 = r.Next(22, 32) + 1 & "; " & r.Next(-9, 1) + 1
                        incorrect2 = r.Next(33, 43) + 1 & "; " & r.Next(2, 12) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 28
                        PcbEjercicio.Image = My.Resources._28
                        correct = "0,17; -0,6"
                        incorrect1 = r.Next(-10, 0) + 1 & "; " & r.Next(-11, -1) + 1
                        incorrect2 = r.Next(1, 11) + 1 & "; " & r.Next(0, 10) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 29
                        PcbEjercicio.Image = My.Resources._29
                        correct = "7,39; 14,78"
                        incorrect1 = r.Next(-3, 7) + 1 & "; " & r.Next(4, 14) + 1
                        incorrect2 = r.Next(8, 16) + 1 & "; " & r.Next(15, 25) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 30
                        PcbEjercicio.Image = My.Resources._30
                        correct = "18; 54"
                        incorrect1 = r.Next(7, 17) + 1 & "; " & r.Next(43, 53) + 1
                        incorrect2 = r.Next(18, 28) + 1 & "; " & r.Next(54, 64) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 31
                        PcbEjercicio.Image = My.Resources._31
                        correct = "835,43; 435,43"
                        incorrect1 = r.Next(825, 835) + 1 & "; " & r.Next(425, 435) + 1
                        incorrect2 = r.Next(836, 846) + 1 & "; " & r.Next(436, 446) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 32
                        PcbEjercicio.Image = My.Resources._32
                        correct = "-568"
                        incorrect1 = r.Next(-579, -569) + 1
                        incorrect2 = r.Next(-568, -558) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 33
                        PcbEjercicio.Image = My.Resources._33
                        correct = "-19428"
                        incorrect1 = r.Next(-19439, -19429) + 1
                        incorrect2 = r.Next(-19428, -19418) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 34
                        PcbEjercicio.Image = My.Resources._34
                        correct = "0,78"
                        incorrect1 = r.Next(-10, 0) + 1
                        incorrect2 = r.Next(1, 11) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 35
                        PcbEjercicio.Image = My.Resources._35
                        correct = "5,83[u]"
                        incorrect1 = r.Next(-5, 5) + 1 & "[u]"
                        incorrect2 = r.Next(6, 16) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 36
                        PcbEjercicio.Image = My.Resources._36
                        correct = "7,07[u]"
                        incorrect1 = r.Next(-3, 7) + 1 & "[u]"
                        incorrect2 = r.Next(8, 18) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 37
                        PcbEjercicio.Image = My.Resources._37
                        correct = "6,48[u]"
                        incorrect1 = r.Next(-4, 6) + 1 & "[u]"
                        incorrect2 = r.Next(7, 17) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 38
                        PcbEjercicio.Image = My.Resources._38
                        correct = "4,58[u]"
                        incorrect1 = r.Next(-6, 4) + 1 & "[u]"
                        incorrect2 = r.Next(5, 15) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 39
                        PcbEjercicio.Image = My.Resources._39
                        correct = "10,34[u]"
                        incorrect1 = r.Next(0, 10) + 1 & "[u]"
                        incorrect2 = r.Next(11, 21) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 40
                        PcbEjercicio.Image = My.Resources._40
                        correct = "13,75[u]"
                        incorrect1 = r.Next(3, 13) + 1 & "[u]"
                        incorrect2 = r.Next(14, 24) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 41
                        PcbEjercicio.Image = My.Resources._41
                        correct = "10,39[u]"
                        incorrect1 = r.Next(0, 10) + 1 & "[u]"
                        incorrect2 = r.Next(11, 21) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 42
                        PcbEjercicio.Image = My.Resources._42
                        correct = "5,66[u]"
                        incorrect1 = r.Next(-5, 5) + 1 & "[u]"
                        incorrect2 = r.Next(6, 16) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 43
                        PcbEjercicio.Image = My.Resources._43
                        correct = "9,54[u]"
                        incorrect1 = r.Next(-1, 9) + 1 & "[u]"
                        incorrect2 = r.Next(10, 20) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 44
                        PcbEjercicio.Image = My.Resources._44
                        correct = "11,58[u]"
                        incorrect1 = r.Next(1, 11) + 1 & "[u]"
                        incorrect2 = r.Next(12, 22) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 45
                        PcbEjercicio.Image = My.Resources._45
                        correct = "19,1[u]"
                        incorrect1 = r.Next(9, 19) + 1 & "[u]"
                        incorrect2 = r.Next(20, 30) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 46
                        PcbEjercicio.Image = My.Resources._46
                        correct = "24,37[u]"
                        incorrect1 = r.Next(14, 24) + 1 & "[u]"
                        incorrect2 = r.Next(25, 35) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 47
                        PcbEjercicio.Image = My.Resources._47
                        correct = "31[u]"
                        incorrect1 = r.Next(20, 30) + 1 & "[u]"
                        incorrect2 = r.Next(31, 41) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 48
                        PcbEjercicio.Image = My.Resources._48
                        correct = "113,58[u]"
                        incorrect1 = r.Next(103, 113) + 1 & "[u]"
                        incorrect2 = r.Next(114, 124) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 49
                        PcbEjercicio.Image = My.Resources._49
                        correct = "146,95[u]"
                        incorrect1 = r.Next(136, 146) + 1 & "[u]"
                        incorrect2 = r.Next(147, 157) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 50
                        PcbEjercicio.Image = My.Resources._50
                        correct = "161,27[u]"
                        incorrect1 = r.Next(151, 161) + 1 & "[u]"
                        incorrect2 = r.Next(162, 172) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 51
                        PcbEjercicio.Image = My.Resources._51
                        correct = "105[u]"
                        incorrect1 = r.Next(94, 104) + 1 & "[u]"
                        incorrect2 = r.Next(105, 115) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 52
                        PcbEjercicio.Image = My.Resources._52
                        correct = "88,57[u]"
                        incorrect1 = r.Next(78, 88) + 1 & "[u]"
                        incorrect2 = r.Next(89, 99) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 53
                        PcbEjercicio.Image = My.Resources._53
                        correct = "60,42[u]"
                        incorrect1 = r.Next(50, 60) + 1 & "[u]"
                        incorrect2 = r.Next(61, 71) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 54
                        PcbEjercicio.Image = My.Resources._54
                        correct = "111,76[u]"
                        incorrect1 = r.Next(101, 111) + 1 & "[u]"
                        incorrect2 = r.Next(112, 122) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 55
                        PcbEjercicio.Image = My.Resources._55
                        correct = "139,3[u]"
                        incorrect1 = r.Next(129, 139) + 1 & "[u]"
                        incorrect2 = r.Next(140, 150) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 56
                        PcbEjercicio.Image = My.Resources._56
                        correct = "102,12[u]"
                        incorrect1 = r.Next(92, 102) + 1 & "[u]"
                        incorrect2 = r.Next(103, 113) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 57
                        PcbEjercicio.Image = My.Resources._57
                        correct = "84,92[u]"
                        incorrect1 = r.Next(74, 84) + 1 & "[u]"
                        incorrect2 = r.Next(85, 95) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 58
                        PcbEjercicio.Image = My.Resources._58
                        correct = "54,36[u]"
                        incorrect1 = r.Next(44, 54) + 1 & "[u]"
                        incorrect2 = r.Next(55, 65) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 59
                        PcbEjercicio.Image = My.Resources._59
                        correct = "101,83[u]"
                        incorrect1 = r.Next(91, 101) + 1 & "[u]"
                        incorrect2 = r.Next(102, 112) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 60
                        PcbEjercicio.Image = My.Resources._60
                        correct = "79,06[u]"
                        incorrect1 = r.Next(69, 79) + 1 & "[u]"
                        incorrect2 = r.Next(80, 90) + 1 & "[u]"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 61
                        PcbEjercicio.Image = My.Resources._61
                        correct = "(-1,-26,0)"
                        incorrect1 = "(" & r.Next(-12, -2) + 1 & "," & r.Next(-37, -27) + 1 & "," & r.Next(-11, -1) + 1 & ")"
                        incorrect2 = "(" & r.Next(-1, 9) + 1 & "," & r.Next(-26, -16) + 1 & "," & r.Next(0, 10) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 62
                        PcbEjercicio.Image = My.Resources._62
                        correct = "(80,-8,9)"
                        incorrect1 = "(" & r.Next(69, 79) + 1 & "," & r.Next(-19, -9) + 1 & "," & r.Next(-2, 8) + 1 & ")"
                        incorrect2 = "(" & r.Next(80, 90) + 1 & "," & r.Next(-8, 2) + 1 & "," & r.Next(9, 19) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 63
                        PcbEjercicio.Image = My.Resources._63
                        correct = "-655; 655"
                        incorrect1 = r.Next(-666, -656) + 1 & "; " & r.Next(644, 654) + 1
                        incorrect2 = r.Next(-655, -645) + 1 & "; " & r.Next(655, 665) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 64
                        PcbEjercicio.Image = My.Resources._64
                        correct = "T=12"
                        incorrect1 = "T=" & r.Next(1, 11) + 1
                        incorrect2 = "T=" & r.Next(12, 22) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 65
                        PcbEjercicio.Image = My.Resources._65
                        correct = "T=0"
                        incorrect1 = "T=" & r.Next(-11, -1) + 1
                        incorrect2 = "T=" & r.Next(0, 10) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 66
                        PcbEjercicio.Image = My.Resources._66
                        correct = "C=0,39"
                        incorrect1 = "C=" & r.Next(-10, 0) + 1
                        incorrect2 = r.Next(1, 11) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 67
                        PcbEjercicio.Image = My.Resources._67
                        correct = "(216,60,63)"
                        incorrect1 = "(" & r.Next(205, 215) + 1 & "," & r.Next(49, 59) + 1 & "," & r.Next(52, 62) + 1 & ")"
                        incorrect2 = "(" & r.Next(216, 226) + 1 & "," & r.Next(60, 70) + 1 & "," & r.Next(63, 73) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 68
                        PcbEjercicio.Image = My.Resources._68
                        correct = "Min(0,0,-3)"
                        incorrect1 = "Min(" & r.Next(-11, -1) + 1 & "," & r.Next(-11, -1) + 1 & "," & r.Next(-14, -4) + 1 & ")"
                        incorrect2 = "Max(" & r.Next(0, 10) + 1 & "," & r.Next(0, 10) + 1 & "," & r.Next(-3, 7) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 69
                        PcbEjercicio.Image = My.Resources._69
                        correct = "Min(-1,-1,-2)"
                        incorrect1 = "Min(" & r.Next(-12, -2) + 1 & "," & r.Next(-12, -2) + 1 & "," & r.Next(-13, -3) + 1 & ")"
                        incorrect2 = "Max(" & r.Next(-1, 9) + 1 & "," & r.Next(-1, 9) + 1 & "," & r.Next(-2, 8) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 70
                        PcbEjercicio.Image = My.Resources._70
                        correct = "Max(2;4;2,72)"
                        incorrect1 = "Max(" & r.Next(-9, 1) + 1 & ";" & r.Next(-7, 3) + 1 & ";" & r.Next(-8, 2) + 1 & ")"
                        incorrect2 = "Min(" & r.Next(2, 12) + 1 & ";" & r.Next(4, 14) + 1 & ";" & r.Next(3, 13) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 71
                        PcbEjercicio.Image = My.Resources._71
                        correct = "Min(1,2,-1)"
                        incorrect1 = "Max(" & r.Next(-10, 0) + 1 & "," & r.Next(-9, 1) + 1 & "," & r.Next(-12, -2) + 1 & ")"
                        incorrect2 = "Min(" & r.Next(1, 11) + 1 & "," & r.Next(2, 12) + 1 & "," & r.Next(-1, 9) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 72
                        PcbEjercicio.Image = My.Resources._72
                        correct = "Min(1,4,-2)"
                        incorrect1 = "Max(" & r.Next(-10, 0) + 1 & "," & r.Next(-7, 3) + 1 & "," & r.Next(-13, -3) + 1 & ")"
                        incorrect2 = "Min(" & r.Next(1, 11) + 1 & "," & r.Next(4, 14) + 1 & "," & r.Next(-2, 8) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 73
                        PcbEjercicio.Image = My.Resources._73
                        correct = "No existe extremo!"
                        incorrect1 = "Max(" & r.Next(-20, 0) + 1 & "," & r.Next(-17, 3) + 1 & "," & r.Next(-23, -3) + 1 & ")"
                        incorrect2 = "Min(" & r.Next(1, 21) + 1 & "," & r.Next(4, 24) + 1 & "," & r.Next(-2, 18) + 1 & ")"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 74
                        PcbEjercicio.Image = My.Resources._74
                        correct = "84,38"
                        incorrect1 = r.Next(74, 84) + 1
                        incorrect2 = r.Next(85, 95) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 75
                        PcbEjercicio.Image = My.Resources._75
                        correct = "28"
                        incorrect1 = r.Next(17, 27) + 1
                        incorrect2 = r.Next(28, 38) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 76
                        PcbEjercicio.Image = My.Resources._76
                        correct = "1"
                        incorrect1 = r.Next(-10, 0) + 1
                        incorrect2 = r.Next(1, 11) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 77
                        PcbEjercicio.Image = My.Resources._77
                        correct = "25(1+z)"
                        incorrect1 = r.Next(14, 24) + 1 & "(" & r.Next(-10, 0) + 1 & r.Next(-10, 0) + 1 & "z)"
                        incorrect2 = r.Next(25, 35) + 1 & "(" & r.Next(1, 11) + 1 & r.Next(1, 11) + 1 & "z)"
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 78
                        PcbEjercicio.Image = My.Resources._78
                        correct = "74,67"
                        incorrect1 = r.Next(64, 74) + 1
                        incorrect2 = r.Next(75, 85) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 79
                        PcbEjercicio.Image = My.Resources._79
                        correct = "13,56"
                        incorrect1 = r.Next(3, 13) + 1
                        incorrect2 = r.Next(14, 24) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 80
                        PcbEjercicio.Image = My.Resources._80
                        correct = "6,39"
                        incorrect1 = r.Next(-4, 6) + 1
                        incorrect2 = r.Next(7, 17) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                    Case 81
                        PcbEjercicio.Image = My.Resources._81
                        correct = "774,4"
                        incorrect1 = r.Next(764, 774) + 1
                        incorrect2 = r.Next(775, 785) + 1
                        alAzarBotonYEjercicio(correct, incorrect1, incorrect2)
                End Select
                GbxEjercicio.Text = "Ejercicio: " & AcontadorS
                btnActual.BackColor = Color.Gray
            End If
        ElseIf btnActual.Text = "9" Then
            btnActual.Text = "1"
        Else
            btnActual.Text = Trim(Str((CInt(btnActual.Text) + 1)))
        End If
        If Not SolutionMostrada Then ' durante cuando no se muestre la solucion
            For i = 1 To 81 ' revise 81 botones continuamente
                If thisbutton(i).BackColor = Color.Orange Then
                    If thisbutton(i).Text <> SolucionSudoku(i) Then 'compara usuario universal con solucion real
                        encontradaSolucion = False ' por lo menos un boton mal, chau
                        Exit For
                    ElseIf thisbutton(i).Text = SolucionSudoku(i) Then
                        encontradaSolucion = True 'si todos botones persisten bien, al final 81 sera un true...
                    End If
                End If
            Next
        End If
        If encontradaSolucion = True Then ' mis 81 boton estan bien!
            TxtGano.Text = "Ganaste!"
            TxtGano.Visible = True
        End If
    End Sub
    Private Sub BtnsRespuesta_Click(sender As System.Object, e As System.EventArgs) Handles BtnRespuesta1.Click, BtnRespuesta2.Click, BtnRespuesta3.Click
        Dim btnActual As Button = CType(sender, Button)
        Dim i As Byte
        BtnAnterior.Enabled = True
        BtnSiguiente.Enabled = True
        For i = 1 To 3
            thisEjbtn(i).Enabled = False
            thisEjbtn(i).BackColor = Color.Red
        Next
        If btnActual.Text = correct Then
            btnActual.BackColor = Color.Green
            chkCorrecto.Checked = True
            correctoOIncorrecto()
        ElseIf btnActual.Text = incorrect1 Or btnActual.Text = incorrect2 Then
            chkCorrecto.Checked = False
            correctoOIncorrecto()
            For i = 1 To 3
                If thisEjbtn(i).Text = correct Then
                    thisEjbtn(i).BackColor = Color.Green
                End If
            Next
        End If
    End Sub
    Private Sub BtnSolucion_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnSolucion.Click
        If TxtPass.Text = "agar.io" Then
            Dim i As Byte
            If SolutionMostrada = False Then ' durante solucion no mostrada y hago click en boton solucion
                BtnSolucion.Text = "Regresar"
                For i = 1 To 81
                    SudokuUsuario(i) = thisbutton(i).Text 'guardar botones del usuario 
                    thisbutton(i).Text = SolucionSudoku(i) 'mostrar la verdadera solucion
                    thisbutton(i).Enabled = False 'imposible modificar la verdadera solucion
                Next
            Else ' durante solucion ya se muestra y hago click en boton solucion...
                BtnSolucion.Text = "Solucion"
                '
                For i = 1 To 81
                    If thisbutton(i).BackColor = Color.LightGreen Or thisbutton(i).BackColor = Color.HotPink Then 'si estaba verde que no se pueda utilizar
                        thisbutton(i).Enabled = False
                    Else 'si no verde muestre lo que ingreso el usuario y que pueda utilizar
                        thisbutton(i).Text = SudokuUsuario(i)
                        thisbutton(i).Enabled = True
                    End If
                Next
                '
            End If
            SolutionMostrada = Not SolutionMostrada 'invertir valor si se muestra o no se muestra
        End If
    End Sub
    Private Function imagenContador()
        Select Case AcontadorS
            Case 1
                PcbEjercicio.Image = My.Resources._1
            Case 2
                PcbEjercicio.Image = My.Resources._2
            Case 3
                PcbEjercicio.Image = My.Resources._3
            Case 4
                PcbEjercicio.Image = My.Resources._4
            Case 5
                PcbEjercicio.Image = My.Resources._5
            Case 6
                PcbEjercicio.Image = My.Resources._6
            Case 7
                PcbEjercicio.Image = My.Resources._7
            Case 8
                PcbEjercicio.Image = My.Resources._8
            Case 9
                PcbEjercicio.Image = My.Resources._9
            Case 10
                PcbEjercicio.Image = My.Resources._10
            Case 11
                PcbEjercicio.Image = My.Resources._11
            Case 12
                PcbEjercicio.Image = My.Resources._12
            Case 13
                PcbEjercicio.Image = My.Resources._13
            Case 14
                PcbEjercicio.Image = My.Resources._14
            Case 15
                PcbEjercicio.Image = My.Resources._15
            Case 16
                PcbEjercicio.Image = My.Resources._16
            Case 17
                PcbEjercicio.Image = My.Resources._17
            Case 18
                PcbEjercicio.Image = My.Resources._18
            Case 19
                PcbEjercicio.Image = My.Resources._19
            Case 20
                PcbEjercicio.Image = My.Resources._20
            Case 21
                PcbEjercicio.Image = My.Resources._21
            Case 22
                PcbEjercicio.Image = My.Resources._22
            Case 23
                PcbEjercicio.Image = My.Resources._23
            Case 24
                PcbEjercicio.Image = My.Resources._24
            Case 25
                PcbEjercicio.Image = My.Resources._25
            Case 26
                PcbEjercicio.Image = My.Resources._26
            Case 27
                PcbEjercicio.Image = My.Resources._27
            Case 28
                PcbEjercicio.Image = My.Resources._28
            Case 29
                PcbEjercicio.Image = My.Resources._29
            Case 30
                PcbEjercicio.Image = My.Resources._30
            Case 31
                PcbEjercicio.Image = My.Resources._31
            Case 32
                PcbEjercicio.Image = My.Resources._32
            Case 33
                PcbEjercicio.Image = My.Resources._33
            Case 34
                PcbEjercicio.Image = My.Resources._34
            Case 35
                PcbEjercicio.Image = My.Resources._35
            Case 36
                PcbEjercicio.Image = My.Resources._36
            Case 37
                PcbEjercicio.Image = My.Resources._37
            Case 38
                PcbEjercicio.Image = My.Resources._38
            Case 39
                PcbEjercicio.Image = My.Resources._39
            Case 40
                PcbEjercicio.Image = My.Resources._40
            Case 41
                PcbEjercicio.Image = My.Resources._41
            Case 42
                PcbEjercicio.Image = My.Resources._42
            Case 43
                PcbEjercicio.Image = My.Resources._43
            Case 44
                PcbEjercicio.Image = My.Resources._44
            Case 45
                PcbEjercicio.Image = My.Resources._45
            Case 46
                PcbEjercicio.Image = My.Resources._46
            Case 47
                PcbEjercicio.Image = My.Resources._47
            Case 48
                PcbEjercicio.Image = My.Resources._48
            Case 49
                PcbEjercicio.Image = My.Resources._49
            Case 50
                PcbEjercicio.Image = My.Resources._50
            Case 51
                PcbEjercicio.Image = My.Resources._51
            Case 52
                PcbEjercicio.Image = My.Resources._52
            Case 53
                PcbEjercicio.Image = My.Resources._53
            Case 54
                PcbEjercicio.Image = My.Resources._54
            Case 55
                PcbEjercicio.Image = My.Resources._55
            Case 56
                PcbEjercicio.Image = My.Resources._56
            Case 57
                PcbEjercicio.Image = My.Resources._57
            Case 58
                PcbEjercicio.Image = My.Resources._58
            Case 59
                PcbEjercicio.Image = My.Resources._59
            Case 60
                PcbEjercicio.Image = My.Resources._60
            Case 61
                PcbEjercicio.Image = My.Resources._61
            Case 62
                PcbEjercicio.Image = My.Resources._62
            Case 63
                PcbEjercicio.Image = My.Resources._63
            Case 64
                PcbEjercicio.Image = My.Resources._64
            Case 65
                PcbEjercicio.Image = My.Resources._65
            Case 66
                PcbEjercicio.Image = My.Resources._66
            Case 67
                PcbEjercicio.Image = My.Resources._67
            Case 68
                PcbEjercicio.Image = My.Resources._68
            Case 69
                PcbEjercicio.Image = My.Resources._69
            Case 70
                PcbEjercicio.Image = My.Resources._70
            Case 71
                PcbEjercicio.Image = My.Resources._71
            Case 72
                PcbEjercicio.Image = My.Resources._72
            Case 73
                PcbEjercicio.Image = My.Resources._73
            Case 74
                PcbEjercicio.Image = My.Resources._74
            Case 75
                PcbEjercicio.Image = My.Resources._75
            Case 76
                PcbEjercicio.Image = My.Resources._76
            Case 77
                PcbEjercicio.Image = My.Resources._77
            Case 78
                PcbEjercicio.Image = My.Resources._78
            Case 79
                PcbEjercicio.Image = My.Resources._79
            Case 80
                PcbEjercicio.Image = My.Resources._80
            Case 81
                PcbEjercicio.Image = My.Resources._81
        End Select
    End Function
    Private Sub BtnAnterior_Click(sender As System.Object, e As System.EventArgs) Handles BtnAnterior.Click
        seVioEjercicioAtras = True
        chkCorrecto.Checked = False
        If (AcontadorS = 1 Or AcontadorS = 0) Then
            AcontadorS = 81
        Else
            AcontadorS = AcontadorS - 1
        End If
        imagenContador()
        GbxEjercicio.Text = "Ejercicio: " & AcontadorS
    End Sub

    Private Sub BtnSiguiente_Click(sender As System.Object, e As System.EventArgs) Handles BtnSiguiente.Click
        seVioEjercicioAdelante = True
        chkCorrecto.Checked = False
        If (AcontadorS = 81) Then
            AcontadorS = 1
        Else
            AcontadorS = AcontadorS + 1
        End If
        imagenContador()
        GbxEjercicio.Text = "Ejercicio: " & AcontadorS
    End Sub
End Class
