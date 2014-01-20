Public Class Form1

    '=============================================================
    '   Utilitaires
    '=============================================================

    Private Function test_erreur()
        If Me.tb_result.Text = "Ohoh ! Division par 0 interdit" Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Sub store_the_fuckin_operand(op As Integer)
        ' La fonction qui sert à rien
        ' mais ca me fait rigoler
        Me.operand = op
    End Sub


    Private Sub i_pushed_a_fuckin_operand(op As Integer)
        If Me.operand <> 0 Then
            Me.do_the_fuckin_operation()
        End If
        Me.store_the_fuckin_operand(op)
    End Sub

    Private Function test_zero(numb As Integer)
        If result(numb) = 0 And tb_buffer.Text <> "0." Then
            Return True
        Else
            Return False
        End If
    End Function

    ' permet de choisir entre result(0) et result(1)
    Private Function choose_number()
        If Me.operand = 0 Then
            Return 0
        Else
            Return 1
        End If
    End Function

    '=============================================================
    '   Choix operation
    '=============================================================
    Private Sub do_the_fuckin_operation()
        If test_erreur() = True Then
            tb_result.Text = ""
        End If
        If Me.operand = 1 Then
            Me.operation_plus(Me.result(0), Me.result(1))
        ElseIf Me.operand = 2 Then
            Me.operation_minus(Me.result(0), Me.result(1))
        ElseIf Me.operand = 3 Then
            Me.operation_star(Me.result(0), Me.result(1))
        ElseIf Me.operand = 4 Then
            Me.operation_slash(Me.result(0), Me.result(1))
        End If
    End Sub

    Private Sub do_fuckin_add(op As String)
        If test_erreur() = True Then
            tb_result.Text = ""
        End If
        If op = "." Then
            Me.add_coma()
        ElseIf op = "sqrt" Then
            Me.add_sqrt()
        ElseIf op = "1/x" Then
            Me.add_inverse()
        ElseIf op = "sign" Then
            Me.add_changesign()
        ElseIf op = "%" Then
            Me.add_pourcent()
        End If
    End Sub

    Private Sub do_fuckin_eraser(era As String)
        If test_erreur() = True Then
            tb_result.Text = ""
        End If
        If era = "<-" Then
            eraser_backspace()
        ElseIf era = "CE" Then
            eraser_cancelled()
        ElseIf era = "C" Then
            eraser_reset()
        End If
    End Sub

    '=============================================================
    '   Operation function
    '=============================================================
    Private Sub operation_plus(a As Double, b As Double)
        Me.result(2) = a + b
        Me.result(0) = Me.result(2)
        Me.result(1) = 0
    End Sub
    Private Sub operation_minus(a As Double, b As Double)
        Me.result(2) = a - b
        Me.result(0) = Me.result(2)
        Me.result(1) = 0
    End Sub
    Private Sub operation_star(a As Double, b As Double)
        Me.result(2) = a * b
        Me.result(0) = Me.result(2)
        Me.result(1) = 0
    End Sub
    Private Sub operation_slash(a As Double, b As Double)
        If b = 0 Then
            Me.tb_result.Text = "Ohoh ! Division par 0 interdit"
            Me.result(2) = 0
        Else
            Me.result(2) = a / b
        End If

        Me.result(0) = Me.result(2)
        Me.result(1) = 0
    End Sub

    Private Sub operation_equal()
        If Me.operand <> 0 Then
            Me.tb_buffer.Text = Me.result(0)
        End If
    End Sub

    '==============================================================================
    '   fonctions faisant l'operation sur le nombre courant
    '==============================================================================

    'ajoute des chiffres a la suite
    Private Sub add_number(numb As Integer)
        If test_zero(choose_number()) = True Or equal_activated = True Or Me.memory <> 0 Then
            equal_activated = False
            Me.tb_buffer.Text = numb
            result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
        Else

            Me.tb_buffer.Text &= numb
            result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
        End If

    End Sub

    Private Sub add_inverse()
        If tb_buffer.Text = 0 Then
            Me.tb_result.Text = "Ohoh ! Division par 0 interdit"
        Else
            Me.tb_buffer.Text = 1 / Double.Parse(Me.tb_buffer.Text)
            Me.result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
        End If
    End Sub

    Private Sub add_operand(op As String)
        If test_erreur() = True Then
            tb_result.Text = ""
        End If
        If Me.operand <> 0 And Me.result(1) = 0 And Me.tb_result.Text.Length <> 0 Then
            Me.tb_result.Text = Me.tb_result.Text.Substring(0, Me.tb_result.Text.Length - 1)
            Me.tb_result.Text &= op
        ElseIf Me.operand <> 0 And equal_activated = False Then
            Me.tb_result.Text &= Me.result(1) & op
            do_the_fuckin_operation()
            Me.tb_buffer.Text = Me.result(0)
        Else
            Me.tb_result.Text &= Me.result(0) & op
        End If
    End Sub

    Private Sub add_coma()
        Dim entier As Integer
        entier = Me.result(choose_number())
        If Me.result(choose_number()) Mod entier = 0 Or Me.result(choose_number()) = 0 Then
            Me.tb_buffer.Text = result(choose_number()) & "."
        End If
    End Sub

    Private Sub add_pourcent()
        Dim inter As Double
        If Me.operand <> 0 Then
            inter = (Me.result(0) * Double.Parse(Me.tb_buffer.Text)) / 100
        Else
            inter = inter = (Me.result(0) * Me.result(0)) / 100
        End If
        Me.tb_buffer.Text = inter
        result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
    End Sub

    Private Sub add_sqrt()
        Me.tb_buffer.Text = Math.Sqrt(Double.Parse(tb_buffer.Text))
        Me.result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
    End Sub

    Private Sub add_changesign()
        Dim numb As Double
        numb = Double.Parse(Me.tb_buffer.Text)
        numb = -numb
        Me.tb_buffer.Text = numb
        Me.result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
    End Sub

    '==============================================================================
    ' functions pour effacer
    '==============================================================================

    Private Sub eraser_backspace()
        If tb_buffer.Text.Length - 1 > 0 And equal_activated = False Then
            tb_buffer.Text = tb_buffer.Text.Substring(0, tb_buffer.Text.Length - 1)
        ElseIf equal_activated = False Then
            tb_buffer.Text = 0
        End If
        result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
    End Sub

    Private Sub eraser_cancelled()
        tb_buffer.Text = 0
        result(choose_number()) = Double.Parse(Me.tb_buffer.Text)
        If equal_activated = True Then
            result(0) = 0
        End If
    End Sub

    Private Sub eraser_reset()
        Me.operand = 0
        Me.result(0) = 0
        Me.result(1) = 0
        Me.result(2) = 0
        tb_buffer.Text = 0
        Me.equal_activated = False
        tb_result.Text = ""
    End Sub
    '==============================================================================
    '   OnClick Number buttons routines
    '==============================================================================
    Private Sub b_zero_Click(sender As Object, e As EventArgs) Handles b_zero.Click
        add_number(0)
    End Sub

    Private Sub b_one_Click(sender As Object, e As EventArgs) Handles b_one.Click
        add_number(1)
    End Sub

    Private Sub b_two_Click(sender As Object, e As EventArgs) Handles b_two.Click
        add_number(2)
    End Sub

    Private Sub b_three_Click(sender As Object, e As EventArgs) Handles b_three.Click
        add_number(3)
    End Sub

    Private Sub b_four_Click(sender As Object, e As EventArgs) Handles b_four.Click
        add_number(4)
    End Sub
    Private Sub b_five_Click(sender As Object, e As EventArgs) Handles b_five.Click
        add_number(5)
    End Sub
    Private Sub b_six_Click(sender As Object, e As EventArgs) Handles b_six.Click
        add_number(6)
    End Sub
    Private Sub b_seven_Click(sender As Object, e As EventArgs) Handles b_seven.Click
        add_number(7)
    End Sub
    Private Sub b_eight_Click(sender As Object, e As EventArgs) Handles b_eight.Click
        add_number(8)
    End Sub
    Private Sub b_nine_Click(sender As Object, e As EventArgs) Handles b_nine.Click
        add_number(9)
    End Sub

    '==============================================================================
    ' special operation
    '==============================================================================

    Private Sub b_coma_Click(sender As Object, e As EventArgs) Handles b_coma.Click
        Me.do_fuckin_add(".")
    End Sub

    Private Sub b_changesign_Click(sender As Object, e As EventArgs) Handles b_changesign.Click
        Me.do_fuckin_add("sign")
    End Sub

    Private Sub b_sqrt_Click(sender As Object, e As EventArgs) Handles b_sqrt.Click
        Me.do_fuckin_add("sqrt")
    End Sub

    Private Sub b_inverse_Click(sender As Object, e As EventArgs) Handles b_inverse.Click
        Me.do_fuckin_add("1/x")
    End Sub

    Private Sub b_pourcent_Click(sender As Object, e As EventArgs) Handles b_pourcent.Click
        Me.do_fuckin_add("%")
    End Sub

    '==============================================================================
    ' gestion boutons operateurs
    '==============================================================================

    Private Sub b_equal_Click(sender As Object, e As EventArgs) Handles b_equal.Click

        If Me.result(1) <> 0 Then
            Me.equal = Me.result(1)
        End If

        Me.result(1) = equal

        do_the_fuckin_operation()
        operation_equal()
       


        If test_erreur() = False Then
            Me.tb_result.Text = ""
        End If

        Me.equal_activated = True

    End Sub

    Private Sub b_plus_Click(sender As Object, e As EventArgs) Handles b_plus.Click
        add_operand("+")
        store_the_fuckin_operand(1)
    End Sub

    Private Sub b_minus_Click(sender As Object, e As EventArgs) Handles b_minus.Click
        add_operand("-")
        store_the_fuckin_operand(2)
    End Sub

    Private Sub b_star_Click(sender As Object, e As EventArgs) Handles b_star.Click
        add_operand("*")
        store_the_fuckin_operand(3)
    End Sub

    Private Sub b_slash_Click(sender As Object, e As EventArgs) Handles b_slash.Click
        add_operand("/")
        store_the_fuckin_operand(4)
    End Sub

    '==============================================================================
    ' gestion boutons effaceurs
    '==============================================================================


    Private Sub b_back_Click(sender As Object, e As EventArgs) Handles b_back.Click
        do_fuckin_eraser("<-")
    End Sub

    Private Sub b_ce_Click(sender As Object, e As EventArgs) Handles b_ce.Click
        do_fuckin_eraser("CE")
    End Sub

    Private Sub b_c_Click(sender As Object, e As EventArgs) Handles b_c.Click
        do_fuckin_eraser("C")
    End Sub

    '==============================================================================
    ' gestion boutons memoire
    '==============================================================================


    Private Sub b_mc_Click(sender As Object, e As EventArgs) Handles b_mc.Click
        Me.memory = 0
    End Sub

    Private Sub b_mr_Click(sender As Object, e As EventArgs) Handles b_mr.Click
        Me.result(choose_number()) = Me.memory
        Me.tb_buffer.Text = Me.memory
        Me.operand = 0
    End Sub

    Private Sub b_ms_Click(sender As Object, e As EventArgs) Handles b_ms.Click
        Me.memory = Double.Parse(Me.tb_buffer.Text)
    End Sub

    Private Sub b_mplus_Click(sender As Object, e As EventArgs) Handles b_mplus.Click
        Me.memory += Double.Parse(Me.tb_buffer.Text)
    End Sub

    Private Sub b_mmoins_Click(sender As Object, e As EventArgs) Handles b_mmoins.Click
        Me.memory += -Double.Parse(Me.tb_buffer.Text)
    End Sub

End Class
