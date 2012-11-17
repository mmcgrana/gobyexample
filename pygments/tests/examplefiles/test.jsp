<!-- This page was loaded on <%= (new java.util.Date()).toLocaleString() %> -->
<%= var x = 1;
%>
<%! int i = 0; %>
<%! int a, b, c; %>
<%! Circle a = new Circle(2.0); %>

<%
      String name = null;
      if (request.getParameter("name") == null) {
%>
<%@ include file="error.html" %>
<%
      } else {
      foo.setName(request.getParameter("name"));
      if (foo.getName().equalsIgnoreCase("integra"))
      name = "acura";
      if (name.equalsIgnoreCase( "acura" )) {
%>

<jsp:useBean id="calendar" scope="page" class="employee.Calendar" />
<h2>
Calendar of <jsp:getProperty name="calendar" property="username" />
</h2>
