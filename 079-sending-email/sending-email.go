// ## Sending Email

package main

import "net/smtp"

func main() {
	auth := smtp.PlainAuth(
		"",
		"mark@heroku.com",
		"xxx",
		"smtp.gmail.com",
	)

	err := smtp.SendMail(
		"smtp.gmail.com:25",
		auth,
		"mark+sent@heroku.com",
		[]string{"mark@heroku.com"},
		[]byte("nThe body."),
	)
	if err != nil { panic(err) }
}

// todo: missing subject, cc, bcc
// todo: attachements
// todo: plain/multi-type emails
// todo: https://github.com/mtoader/google-go-lang-idea-plugin/issues/112
