Public Class Form1
    Inherits System.Windows.Forms.Form

    Private t As New System.Timers.Timer(2000)

    Private Sub Form1_Load(ByVal sender As Object, _
          ByVal e As System.EventArgs) Handles MyBase.Load
        
        AddHandler t.Elapsed, AddressOf TimerFired
    End Sub

    Private Sub btnStart_Click(ByVal sender As System.Object, _ 
          ByVal e As System.EventArgs) Handles btnStart.Click
          
        t.Enabled = True
    End Sub

    Private Sub btnStop_Click(ByVal sender As System.Object, _
          ByVal e As System.EventArgs) Handles btnStop.Click
          
        t.Enabled = False
    End Sub

    Public Sub TimerFired(ByVal sender As Object, _ 
           ByVal e As System.Timers.ElapsedEventArgs)
           
        Label1.Text = "Signal Time = " & e.SignalTime.ToString
    End Sub
End Class
