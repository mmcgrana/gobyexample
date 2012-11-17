<%@ Register TagPrefix="Acme" TagName="Message" Src="userctrl2_vb.ascx" %>

<html>

  <script language="VB" runat="server">

      Sub SubmitBtn_Click(Sender As Object, E As EventArgs)
          MyMessage.MessageText = "Message text changed!"
          MyMessage.Color = "red"
      End Sub

  </script>

<body style="font: 10pt verdana">

  <h3>A Simple User Control w/ Properties</h3>

  <form runat="server">

    <Acme:Message id="MyMessage" MessageText="This is a custom message!" Color="blue" runat="server"/>

    <p>

    <asp:button text="Change Properties" OnClick="SubmitBtn_Click" runat=server/>

  </form>

</body>
</html>
