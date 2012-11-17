<%!-- defined in example/PersonCSVTemplate.gst --%>

<%@ params( users : Collection <User> ) %>

<%  for( user in users ) { %>

${user.LastName}, ${user.FirstName}, ${user.Department}  <%  } %>